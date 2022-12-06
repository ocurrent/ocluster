# OCluster

OCluster manages a pool of build workers.
A build scheduler service accepts build jobs from clients and distributes them to worker machines using Cap'n Proto.
Workers register themselves by connecting to the scheduler (and workers do not need to be able to accept incoming network connections).

The scheduler can manage multiple pools (e.g. `linux-x86_64` and `linux-arm32`).
Clients say which pool should handle their requests.
At the moment, two build types are provided: building a Dockerfile, or building an [OBuilder][] spec.
In either case, the build may done in the context of some Git commit.
The scheduler tries to schedule similar builds on the same machine, to benefit from caching.

## Contents

<!-- vim-markdown-toc GFM -->

* [Installation](#installation)
* [The scheduler service](#the-scheduler-service)
* [Workers](#workers)
* [Clients](#clients)
	* [Docker jobs](#docker-jobs)
	* [OBuilder jobs](#obuilder-jobs)
* [Admin](#admin)
	* [Fair queuing](#fair-queuing)
* [API](#api)
* [Security model](#security-model)
* [Prometheus metrics](#prometheus-metrics)
* [Testing](#testing)

<!-- vim-markdown-toc -->

## Installation

To install the Git version:

```bash
git clone https://github.com/ocurrent/ocluster.git
cd ocluster
opam pin add -yn .
opam depext -i ocluster
```

## The scheduler service

To run the scheduler:

```
mkdir capnp-secrets
ocluster-scheduler \
  --capnp-secret-key-file=./capnp-secrets/key.pem \
  --capnp-listen-address=tcp:0.0.0.0:9000 \
  --capnp-public-address=tcp:127.0.0.1:9000 \
  --state-dir=/var/lib/ocluster-scheduler \
  --pools=linux-arm32,linux-x86_64 \
  --verbose
```

Replace the last line with whatever build pools you want. The names can be anything you like.

Change `capnp-public-address` to an address at which remote machines can reach the port
given in the `listen` address. e.g. change `127.0.0.1` to a public IP address.

`--state-dir` can be anywhere you like.
The scheduler keeps a database here remembering which workers have cached which builds,
as well as the list of clients permitted to use the service.

You should see output something like this:

```
2020-11-05 16:28.30      capnp-rpc [INFO] Generating new secret key to store in "./capnp-secrets/key.pem"
2020-11-05 16:28.30      capnp-rpc [INFO] Generating new private key…
2020-11-05 16:28.30      capnp-rpc [INFO] Generated key with hash sha-256@0_tVPHQ3gz5vKNVwc_t0llFS7YkVuKmqfG7fO9S-gEg
2020-11-05 16:28.30      capnp-rpc [INFO] Waiting for (encrypted) connections on tcp:0.0.0.0:9000
2020-11-05 16:28.30    application  Wrote capability reference to "./capnp-secrets/admin.cap"
2020-11-05 16:28.30    application  Wrote capability reference to "./capnp-secrets/pool-linux-arm32.cap"
2020-11-05 16:28.30    application  Wrote capability reference to "./capnp-secrets/pool-linux-x86_64.cap"
```

The service creates `.cap` files in `./capnp-secrets/` that can be used to connect to the service.
There is one for each named pool (that workers use to register themselves), plus
`admin.cap` for managing the service.

The provided [Dockerfile](./Dockerfile) can be used to build the scheduler service as a Docker image.

## Workers

Each worker needs access to the `.cap` file for its pool. This tells it how to connect, and
contains a secret token granting it access.

To run the build service locally:

```
ocluster-worker \
  --connect=./capnp-secrets/pool-linux-x86_64.cap \
  --state-dir=/var/lib/ocluster-worker \
  --name=my-host --capacity=1 --prune-threshold=20 \
  --verbose
```

Each builder must be given a unique name.
`capacity` controls how many jobs this worker will build at once.
`prune-threshold` says how much free space must be available in the
cache partition (e.g. `/var/lib/docker` for Docker builds) before the worker
will prune things (e.g. with `docker system prune -af`).
If not given, then the worker will not monitor free space.

The builder connects to the scheduler and waits for jobs.
You should see `worker [INFO] Requesting a new job…` in the worker log,
and `Registered new worker "my-host"` in the scheduler log.

The service builds Docker jobs using `docker build` and so needs access to the local Docker.
[Dockerfile.worker](./Dockerfile.worker) can be used to run the worker service itself in a Docker container,
provided that you share this socket with the container.

However, if you intend to support OBuilder jobs, you will probably want to run it directly on the host instead,
as root.

## Clients

To submit a job, you need:

- a `submission.cap` file (granting you permission to submit jobs),
- the name of the pool to use, and
- a description of the job (depending on the job type; see below).

The service administrator can generate a `submission.cap` from the `admin.cap` like this:

```bash
ocluster-admin --connect ./capnp-secrets/admin.cap add-client test-user > submission.cap
```

There is a command-line client, and a plugin for use in [OCurrent](https://github.com/ocurrent/ocurrent) pipelines.
See [obuilder_pipeline.ml](./examples/obuilder_pipeline.ml) for an example pipeline using the plugin.

You might want to create an alias for the admin and submission clients, e.g.

```bash
alias mycluster-admin='ocluster-admin --connect /path/to/admin.cap'
alias mycluster-client='ocluster-client --connect /path/to/submission.cap'
```

### Docker jobs

To submit a Docker job, you also need a `Dockerfile` describing what to build.

You can run a job like this:

```
echo -e "FROM busybox\nRUN date\n" > Dockerfile.test
ocluster-client -c submission.cap \
  submit-docker \
  --local-dockerfile Dockerfile.test \
  --pool=linux-x86_64 \
  --cache-hint tutorial
```

The client will display the log output from the job as it arrives.

The scheduler will remember the cache-hint. If you submit the job again with
the same hint, it will try to schedule it on the same worker, which will be
faster.

Unlike `docker build`, it does not transfer the current directory as the build context.
Instead, you can give a Git repository and a commit. The worker will clone the repository
and checkout the commit, using that as the Docker build context. e.g.

```
ocluster-client -c submission.cap \
  submit-docker \
  --local-dockerfile Dockerfile.test \
  --pool=linux-x86_64 \
  --cache-hint tutorial \
  https://github.com/ocurrent/ocluster.git ac619d2083bb15e0c408e4cd0e3ef7135670cfd5
```

Instead of using `--local-dockerfile`, you could also use `--context-dockerfile=Dockerfile` to
tell the builder to read the Dockerfile from the source commit (this is the default if you don't
specify `--local-dockerfile`).

If you list multiple commit hashes then the builder will merge them together.
This is useful for e.g. testing a pull request merged with the master branch's head.

You can ask the builder to push the resulting image somewhere. The client provides three options for this:

```
ocluster-client … \
  --push-to org/staging:build-1 \
  --push-user=builder \
  --push-password ~/.builder-password
```

You should not give the builders access to your main password or repositories. Instead, create a new user
and a "staging" repository on Docker Hub for this purpose. The builder will return the RepoId of the newly
pushed image, which you can then download, or republish under its final name. You can also combine images
from several builders to create a single multi-arch image using `docker manifest`.

For now, you must also start the builders with `--allow-push org/staging`. This
is because the `docker push` command requires the builder to tag the image
locally before pushing, and it would be unfortunate if a user asked it to tag
an image as e.g. `ocurrent/ocluster-worker:latest`.

The client is responsible for deleting the image once it is no longer needed.

### OBuilder jobs

This is similar to submitting a Docker job, except that you provide an [OBuilder][] spec file
instead of a Dockerfile, e.g.

```
echo -e '((from busybox) (shell /bin/sh -c) (run (shell date)))' > OBuilder.test
ocluster-client -c submission.cap \
  submit-obuilder \
  --local-file OBuilder.test \
  --pool=linux-x86_64 \
  --cache-hint tutorial
```

You will need to start the worker with `--obuilder-store=…` to enable this.

## Admin

The `ocluster-admin` executable can be used to manage the service using `admin.cap`.

To grant a user access to the cluster (the name can be any unique ID):

```
ocluster-admin -c ./capnp-secrets/admin.cap add-client alice > alice.cap
```

You can also use `remove-client` to deactivate the `.cap` file, and `list-clients` to
show all registered users.

For automated deployment scripts, you can also start the scheduler with e.g.
`--default-clients=alice,bob` to create `alice.cap` and `bob.cap` automatically
if they don't already exist.

To get a list of the available pools:

```
ocluster-admin -c ./capnp-secrets/admin.cap show
```

To show the state of one pool:

```
ocluster-admin -c ./capnp-secrets/admin.cap show linux-x86_64
```

To pause a worker, give the pool and the worker's name, e.g.:

```
ocluster-admin -c ./capnp-secrets/admin.cap pause linux-x86_64 my-host
```

A paused worker will not be assigned any more items until it is unpaused, but
it will continue with any jobs it is already running. Use `unpause` to resume it.

Use `pause --wait` if you want to wait until all running jobs have finished.

Instead of specifying a worker, you can also use `--all` to pause or unpause all workers in a pool.

If you want to set the state of a worker that hasn't ever connected to the scheduler, use `--auto-create`.

To update all workers in a pool:

```
ocluster-admin -c ./capnp-secrets/admin.cap update linux-x86_64
```

This will test the update by restarting one worker first. If that succeeds, it will restart the others in
parallel. Note that restarting a worker involves letting it finish any jobs currently in progress, so this
may take a while. The `update` command waits until the worker has reconnected before reporting success.
Note that the worker just exits, assuming a service manager will restart it. If you're testing it manually,
you'll need to restart the worker yourself at this point.

You can also give the name of a worker as an extra argument to update just that worker.

To forget about an old worker (so that it no longer shows up under `disconnected:` in the state display:

```
ocluster-admin -c ./capnp-secrets/admin.cap forget linux-x86_64 my-host
```

### Fair queuing

Some clients may submit many jobs in batches.
In this case, we probably want other client's jobs to enter the queue ahead of them, even if they are submitted afterwards.

To handle this, each client (separately for each pool) can be configured with a "rate",
which is the rate at which they can expect to use the cluster (the number of jobs they will have running at once).
If the cluster has free capacity then this has no effect; all jobs will be run.
However, when queuing the scheduler will use this information to try to schedule jobs so that they run when they
would have run if the client was using the cluster at the expected rate.

For example:

1. The admin sets Alice's rate to 2 and Bob's rate the 1 (the default), using `ocluster-admin set-rate`.
2. Bob submits 10 jobs, each estimated to run for 1 minute.
3. Because Bob's rate is one, the cluster assigns these jobs "fair start times" of now, +1m, +2m, +3m, etc.
4. The cluster will start as many of these jobs as it has capacity for. If its capacity is 3, Bob's first three jobs will start.
5. Alice submits two jobs, also taking one minute each.
   The cluster assigns these jobs fair start times of now and now+30s.
   These will be the next two jobs to run, because their start times are before all of Bob's jobs.

You can see these values in the `ocluster-admin show` output. For example:

```
…
queue: (backlog) [bob:job9@9m bob:job8@8m bob:job7@7m bob:job6@6m bob:job5@5m
                  bob:job4@4m bob:job3@3m alice:job1@30s alice:job0@0s]
clients: alice(2)+1m bob(1)+10m
```

This means that:

- The backlog contains Bob's remaining 7 jobs, queued up behind Alice's 2 jobs.
  `bob:job9@9m` means that Bob's "job9" has a fair start time 9 minutes from now.
  Alice's jobs will go next, because they have earlier start times.
- Alice has a rate of 2 jobs (2 job-seconds per second) and her next job will have a fair start time
  of 1 minute from now (because she has already submitted 2 minutes of jobs and is expected to
  run 2 jobs at once).
- Bob has a rate of 1 and a 10 minute penalty.


## API

To write your own clients or builders:

- [api/schema.capnp](./api/schema.capnp) is the Cap'n Proto schema file.
- The `api` directory contains some OCaml wrappers for this.

## Security model

The worker process has access to the local machine's Docker socket, which effectively gives it root on that machine.
So a compromised build agent compromises that build machine.
A malicious builder can give incorrect results for build jobs
and (by running jobs very quickly) could end up handling most of the jobs within its pool.
Taking over one worker machine should not help in taking over any other machine.
Workers cannot submit jobs to other machines.

A compromised build scheduler can give incorrect results for any job, but
should not be able to compromise the workers or get root on the build machines.
It does not have access to its own machine's docker socket, and should
therefore not be able to compromise its host (assuming it's running in a container).

Users (with access to the submission endpoint) can only submit jobs, and should not
be able to use this to compromise the scheduler or the worker services. Users should
not be able to compromise the jobs of other users. However, if your job requests
use of a shared cache then be aware that other jobs could write to that cache, so the
contents may not be trustworthy.

All communication uses TLS. The `.cap` files contain both the fingerprint of scheduler's key
(allowing clients and builders to verify that they are connected to the real
service) and a secret granting them access to that particular object.

There is no attempt at DoS protection from malicious clients or services.

## Prometheus metrics

You can run the scheduler with `--listen-prometheus=PORT` to expose Prometheus metrics on the given port.
The endpoints available are:

- `http://…:PORT/metrics` provides the metrics for the scheduler itself,
- `http://…:PORT/pool/{pool}/worker/{worker}/metrics` provides the metrics for the given worker
- `http://…:PORT/pool/{pool}/worker/{worker}/host-metrics` gets the worker's prometheus-node-exporter metrics

The worker agent and worker host metrics are fetched over the worker's Cap'n Proto connection, so there is no need
to allow incoming network connections to the workers for this.

## Testing

The `stress` directory contains some code for running the scheduler with some simulated workloads and monitoring it.
To save building things again, `stress/docker-compose.yml` runs the previously-built scheduler binary directly from `_build`,
which therefore needs to be compatible with the `stress/Dockerfile` environment (so edit that if it doesn't match your
main dev platform).

To run the testbed:

```
dune build
cd stress
docker-compose up
```

This runs three services:
- `scheduler` is a scheduler instance with some dummy workers that handle jobs by just sleeping for a bit,
  and some dummy clients that submit jobs to the cluster.
- `prometheus` collects metrics from the scheduler.
- `grafana` displays the metrics.

Open <http://localhost:3000> in a web-browser to view the grafana dashboard for the tests.

The `scheduler` service also writes out `./capnp-secrets/stress-admin.cap`, which you can use from the host to manage the test cluster.
For example:

```
dune exec -- ocluster-admin show -c ./capnp-secrets/stress-admin.cap linux-x86_64
```

You can also create your own client endpoints and submit your own jobs, in addition to those submitted by the testbed itself.

[OBuilder]: https://github.com/ocurrent/obuilder

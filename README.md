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

* [The scheduler service](#the-scheduler-service)
* [Workers](#workers)
* [Clients](#clients)
	* [Docker jobs](#docker-jobs)
	* [OBuilder jobs](#obuilder-jobs)
* [Admin](#admin)
* [API](#api)
* [Security model](#security-model)
* [Prometheus metrics](#prometheus-metrics)

<!-- vim-markdown-toc -->

## The scheduler service

To run the scheduler:

```
mkdir capnp-secrets
dune exec -- ocluster-scheduler \
  --capnp-secret-key-file=./capnp-secrets/key.pem \
  --capnp-listen-address=tcp:0.0.0.0:9000 \
  --capnp-public-address=tcp:127.0.0.1:9000 \
  --state-dir=/var/lib/ocluster-scheduler \
  --pools=linux-arm32,linux-x86_64
```

Replace the last line with whatever build pools you want. The names can be anything you like.

Change `capnp-public-address` to an address at which remote machines can reach the port
given in the `listen` address. e.g. change `127.0.0.1` to a public IP address.

You should see output something like this:

```
2020-11-05 16:28.30      capnp-rpc [INFO] Generating new secret key to store in "./capnp-secrets/key.pem"
2020-11-05 16:28.30      capnp-rpc [INFO] Generating new private key...
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
dune exec -- ocluster-worker ./capnp-secrets/pool-linux-x86_64.cap \
  --state-dir=/var/lib/ocluster-worker \
  --name=my-host --capacity=1 --prune-threshold=20
```

Each builder must be given a unique name.
`capacity` controls how many jobs this worker will build at once.
`prune-threshold` says how much free space must be available in the
`/var/lib/docker` partition before the worker will run `docker system prune -af`.
If not given, then the worker will not monitor free space.

The builder connects to the scheduler and waits for jobs.
You should see `worker [INFO] Requesting a new job...` in the worker log,
and `Registered new worker "my-host"` in the scheduler log.

The service builds the jobs using `docker build` and so needs access to the local Docker.

[Dockerfile.worker](./Dockerfile.worker) can be used to run the worker service itself in a Docker container.
However, if you intend to support OBuilder jobs, you will probably want to run it directly on the host instead.

## Clients

To submit a job, you need:

- a `submission.cap` file (granting you permission to submit jobs),
- the name of the pool to use, and
- a description of the job (depending on the job type; see below).

The service administrator can generate a `submission.cap` from the `admin.cap` like this:

```
dune exec -- ocluster-admin add-client ./capnp-secrets/admin.cap test-user > submission.cap
```

There is a command-line client, and a plugin for use in [OCurrent](https://github.com/ocurrent/ocurrent) pipelines.

### Docker jobs

To submit a Docker job, you also need a `Dockerfile` describing what to build.

You can run a job like this:

```
echo -e "FROM busybox\nRUN date\n" > Dockerfile.test
dune exec -- ocluster-client \
  submit-docker submission.cap \
  --cache-hint tutorial \
  --pool=linux-x86_64 \
  --local-dockerfile Dockerfile.test
```

The client will display the log output from the job as it arrives.

The scheduler will remember the cache-hint. If you submit the job again with
the same hint, it will try to schedule it on the same worker, which will be
faster.

Unlike `docker build`, it does not transfer the current directory as the build context.
Instead, you can give a Git repository and a commit. The worker will clone the repository
and checkout the commit, using that as the Docker build context. e.g.

```
dune exec -- ocluster-client \
  submit-docker submission.cap \
  --cache-hint tutorial \
  --pool=linux-x86_64 \
  --local-dockerfile Dockerfile \
  https://github.com/ocurrent/ocluster.git ac619d2083bb15e0c408e4cd0e3ef7135670cfd5
```

Instead of using `--local-dockerfile`, you could also use `--context-dockerfile=Dockerfile` to
tell the builder to read the Dockerfile from the source commit (this is the default if you don't
specify `--local-dockerfile`).

If you list multiple commit hashes then the builder will merge them together.
This is useful for e.g. testing a pull request merged with the master branch's head.

You can ask the builder to push the resulting image somewhere. The client provides three options for this:

```
ocluster-client ... \
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
dune exec -- ocluster-client \
  submit-obuilder submission.cap \
  --cache-hint tutorial \
  --pool=linux-x86_64 \
  --local-file OBuilder.test
```

## Admin

The `ocluster-admin` executable can be used to manage the service using `admin.cap`.

To grant a user access to the cluster (the name can be any unique ID):

```
dune exec -- ocluster-admin add-client ./capnp-secrets/admin.cap alice > alice.cap
```

You can also use `remove-client` to deactivate the `.cap` file, and `list-clients` to
show all registered users.

To get a list of the available pools:

```
dune exec -- ocluster-admin \
  show ./capnp-secrets/admin.cap
```

To show the state of one pool:

```
dune exec -- ocluster-admin \
  show ./capnp-secrets/admin.cap linux-x86_64
```

To pause a worker, give the pool and the worker's name, e.g.:

```
dune exec -- ocluster-admin \
  pause ./capnp-secrets/admin.cap linux-x86_64 my-host
```

A paused worker will not be assigned any more items until it is unpaused, but
it will continue with any jobs it is already running. Use `unpause` to resume it.
Note that if a paused worker disconnects and reconnects, it will be unpaused automatically.

Instead of specifying a worker, you can also use `--all` to pause or unpause all workers in a pool.

To update all workers in a pool:

```
dune exec -- ocluster-admin \
  update ./capnp-secrets/admin.cap linux-x86_64
```

This will test the update by restarting one worker first. If that succeeds, it will restart the others in
parallel. Note that restarting a worker involves letting it finish any jobs currently in progress, so this
may take a while. The `restart` command waits until the worker has reconnected before reporting success.

You can also give the name of a worker as an extra argument to update just that worker.


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

- `http://...:PORT/metrics` provides the metrics for the scheduler itself,
- `http://...:PORT/pool/{pool}/worker/{worker}/metrics` provides the metrics for the given worker
- `http://...:PORT/pool/{pool}/worker/{worker}/host-metrics` gets the worker's prometheus-node-exporter metrics

The worker agent and worker host metrics are fetched over the worker's Cap'n Proto connection, so there is no need
to allow incoming network connections to the workers for this.

[OBuilder]: https://github.com/ocurrent/obuilder

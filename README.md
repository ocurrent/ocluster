# Build scheduler

Status: **experimental**

A build scheduler service that accepts build jobs from clients and distributes them to worker machines using Cap'n Proto.
Workers register themselves by connecting to the scheduler (and workers do not need to be able to accept incoming network connections).

## The scheduler service

To run the scheduler:

```
mkdir capnp-secrets
dune exec -- build-scheduler \
  --capnp-secret-key-file=./capnp-secrets/key.pem \
  --capnp-listen-address=tcp:0.0.0.0:9000 \
  --capnp-public-address=tcp:127.0.0.1:9000 \
  --state-dir=/var/lib/build-scheduler \
  --pools=linux-arm32,linux-x86_64
```

Replace the last line with whatever build pools you want. The names can be anything you like.

Change `capnp-public-address` to an address at which remote machines can reach the port
given in the `listen` address. e.g. change `127.0.0.1` to a public IP address.

You should see output something like this:

```
2020-07-07 16:24.28      capnp-rpc [INFO] Generating new secret key to store in "./capnp-secrets/key.pem"
2020-07-07 16:24.28      capnp-rpc [INFO] Generating new private key...
2020-07-07 16:24.28      capnp-rpc [INFO] Generated key with hash sha-256@gTYMtfvMQJJCRrdqzmBxB-fyJYDmB7oXpMuQd6eGoZw
2020-07-07 16:24.28      capnp-rpc [INFO] Waiting for (encrypted) connections on tcp:0.0.0.0:9000
2020-07-07 16:24.28    application  Wrote capability reference to "./capnp-secrets/submission.cap"
2020-07-07 16:24.28    application  Wrote capability reference to "./capnp-secrets/admin.cap"
2020-07-07 16:24.28    application  Wrote capability reference to "./capnp-secrets/pool-linux-arm32.cap"
2020-07-07 16:24.28    application  Wrote capability reference to "./capnp-secrets/pool-linux-x86_64.cap"
```

The service creates `.cap` files in `./capnp-secrets/` that can be used to connect to the service.
There is one for each named pool (that workers use to register themselves),
`submission.cap` for the job submission endpoint (for clients), and
`admin.cap` for managing the service.

[Dockerfile](./Dockerfile) can be used to run the scheduler service in a Docker container.

## Workers

Each worker needs access to the `.cap` file for its pool. This tells it how to connect, and
contains a secret token granting it access.

To run the build service locally:

```
dune exec -- build-worker ./capnp-secrets/pool-linux-x86_64.cap --name=my-host --capacity=1
```

Each builder must be given a unique name.
`capacity` controls how many jobs this worker will build at once.

The builder connects to the scheduler and waits for jobs.
You should see `worker [INFO] Requesting a new job...` in the worker log,
and `Registered new worker "my-host"` in the scheduler log.

The service builds the jobs using `docker build` and so needs access to the local Docker.

[Dockerfile.worker](./Dockerfile.worker) can be used to run the worker service itself in a Docker container.

## Clients

To submit a job, you need:

- the `submission.cap` from the scheduler (granting you permission to submit jobs),
- the name of the pool to use, and
- a `Dockerfile` describing what to build.

You can run a job like this:

```
echo -e "FROM busybox\nRUN date\n" > Dockerfile.test
dune exec -- build-client \
  submit ./capnp-secrets/submission.cap \
  --pool=linux-x86_64 \
  Dockerfile.test
```

The client will display the log output from the job as it arrives.

Unlike `docker build`, it does not transfer the current directory as the build context.
Instead, you can give a Git repository and a commit. The worker will clone the repository
and checkout the commit, using that as the Docker build context. e.g.

```
dune exec -- build-client \
  submit ./capnp-secrets/submission.cap \
  --pool=linux-x86_64 \
  Dockerfile \
  https://github.com/ocurrent/build-scheduler.git cf4b43be2739b0d41924890a63e7a1fa5a2f1e3c
```

If you list multiple commit hashes then the builder will merge them together.
This is useful for e.g. testing a pull request merged with the master branch's head.

The client executable can also be used to manage the service using `admin.cap`.
To get a list of the available pools:

```
dune exec -- build-client \
  show ./capnp-secrets/admin.cap
```

To show the state of one pool:

```
dune exec -- build-client \
  show ./capnp-secrets/admin.cap linux-x86_64
```

### Publishing the result

You can ask the builder to push the resulting image somewhere. The client provides three options for this:

```
build-client ... \
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
an image as e.g. `ocurrent/build-worker:latest`.

The client is responsible for deleting the image once it is no longer needed.

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
not be able to compromise the jobs of other users, unless the builders are configured
to use Docker BuildKit, in which case poisoning of BuildKit's build caches may
be possible.

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

The worker agent and worker host metrics are fetched over the workers Cap'n Proto connection, so there is no need
to allow incoming network connections to the workers for this.

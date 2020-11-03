@0x8378b104cef5b2eb;

struct DockerBuild {
  dockerfile   :union {
    contents   @0 :Text;
    # The contents of the Dockerfile to build.

    path       @5 :Text;
    # The path of the Dockerfile within the context.
  }

  pushTarget   @1 :Text;
  # If set, the builder will "docker push" to this target on success.
  # The format is "repo:tag". The tag is not optional.
  # You'll need to provide pushUser and pushPassword too when using this.
  # To avoid sharing important credentials, you can create a new Docker Hub
  # user for this and push to a staging repository. Then use the returned
  # RepoId hash to tag it in the final repository yourself.
  # Example value: "myorg/staging:job-123"

  pushUser     @2 :Text;
  pushPassword @3 :Text;

  buildArgs    @4 :List(Text);
  # Options to pass to `docker build` using `--build-arg`.

  squash       @6 :Bool;
  # Squash the image layers together using `--squash`.

  buildkit     @7 :Bool;
  # Use BuildKit for the build.
  # Note: buildkit builds shared caches, so clients using such builds must all
  # trust each other not to poison the caches.

  includeGit   @8 :Bool;
  # If set, include the .git directory in the build context.
}

struct OBuilder {
  spec @0 :Text;
  # The contents of the OBuilder spec to build.
}

struct JobDescr {
  action :union {
    dockerBuild @0 :DockerBuild;
    obuilder    @4 :OBuilder;
  }

  cacheHint @1 :Text;
  # Try to place jobs with the same cache_hint on the same node.
  # This will probably be a hash of the first few lines of the Dockerfile.

  repository @2 :Text;
  # The URL of a Git repository with the commit(s) to use as the context.

  commits @3 :List(Text);
  # The commit(s) to use as the context. If the list is empty, there will be no context.
  # If there are multiple items, they will be merged.
}

interface Job {
  log     @0 (start :Int64) -> (log :Data, next :Int64);
  # Return a chunk of log data starting at byte offset "start" and the
  # value to use for "start" in the next call to continue reading.
  # Returns 0 bytes of data to indicate end-of-file.
  # If the log is incomplete and there is no data currently available,
  # it waits until something changes before returning.
  # If "start" is negative then it is relative to the end of the log.

  result @1 () -> (output :Text);
  # Waits for the job to finish. Resolves to an error if the job fails.
  # The output depends on the job type. For a "docker push", it is the RepoId of
  # the pushed image.

  cancel @2 () -> ();
  # Request that the job be cancelled.
  # Note: jobs will be also cancelled if their reference count reaches zero.
}

interface Queue {
  pop       @0 (job :Job) -> (descr :JobDescr);

  setActive @1 (active :Bool) -> ();
  # When a queue is made inactive any items on it are returned to the main
  # queue and no new items will be added until it is made active again. This
  # is useful if a worker needs to stop handling jobs for a while (e.g. to
  # prune or upgrade), but still wants to provide metrics.
}

interface Worker {
  enum MetricsSource {
    agent @0;   # Report the agent's own metrics
    host  @1;   # Get the metrics from the prometheus-node-exporter service
  }

  metrics    @0 (source :MetricsSource) -> (contentType :Text, data :Text);
  # Get Prometheus metrics.

  selfUpdate @1 () -> ();
  # Finish any running jobs and then restart, running the latest version.
  # This call does not return. Instead, it will fail when the jobs have
  # finished and the worker reconnects.
}

interface Registration {
  register @0 (name :Text, worker :Worker) -> (queue :Queue);
}

interface Ticket {
  job    @0 () -> (job :Job);
  # The job object at the build node. This will be a promise until the job has been accepted.

  cancel @1 () -> ();
  # Cancel the job request. If the job has already been assigned to a worker, this cancels
  # the job itself.
}

interface Submission {
  submit @0 (pool :Text, descr :JobDescr, urgent :Bool) -> (ticket :Ticket);
}

struct WorkerInfo {
  name   @0 :Text;
  active @1 :Bool;
}

interface PoolAdmin {
  show      @0 () -> (state :Text);
  workers   @1 () -> (workers : List(WorkerInfo));
  worker    @3 (worker :Text) -> (worker :Worker);
  setActive @2 (worker :Text, active :Bool) -> ();

  update    @4 (worker :Text) -> ();
  # Drain worker, ask it to restart with the latest version, and return when it comes back.
}

interface Admin {
  pools @0 () -> (names :List(Text));
  pool  @1 (name :Text) -> (pool :PoolAdmin);
}

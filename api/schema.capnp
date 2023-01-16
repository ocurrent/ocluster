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
  # You'll probably want to provide pushUser and pushPassword too when using this.
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

struct Custom {
  kind @0 :Text;
  # A name describing the kind of custom job

  payload @1 :AnyPointer;
  # A custom job with a dynamic payload
}

struct Secret {
  id @0 :Text;
  # The secret id.

  value @1 :Text;
  # The secret value.
}

struct JobDescr {
  action :union {
    dockerBuild @0 :DockerBuild;
    obuilder    @4 :OBuilder;
    custom      @6 :Custom;
  }

  cacheHint @1 :Text;
  # Try to place jobs with the same cache_hint on the same node.
  # This will probably be a hash of the first few lines of the Dockerfile.

  repository @2 :Text;
  # The URL of a Git repository with the commit(s) to use as the context.

  commits @3 :List(Text);
  # The commit(s) to use as the context. If the list is empty, there will be no context.
  # If there are multiple items, they will be merged.

  secrets @5 :List(Secret);
  # Secret id-value pairs provided to the job.
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

struct Metric {
  contentType @0 :Text;
  data        @1 :Text;
}

struct AdditionalMetric {
  union {
    metric @0 :Metric;
    notReported @1 :Void;
  }
}

interface Worker {
  enum MetricsSource {
    agent @0;   # Report the agent's own metrics
    host  @1;   # Get the metrics from the prometheus-node-exporter service
  }

  metrics    @0 (source :MetricsSource) -> (contentType :Text, data :Text);
  # Get Prometheus metrics.

  additionalMetric @2 (source :Text) -> (metric: AdditionalMetric);
  # Additional prometheus metrics reported by the worker

  selfUpdate @1 () -> ();
  # Finish any running jobs and then restart, running the latest version.
  # This call does not return. Instead, it will fail when the jobs have
  # finished and the worker reconnects.
}

interface Registration {
  register @0 (name :Text, worker :Worker, capacity: Int32) -> (queue :Queue);
  # Workers call this at startup to register themselves with the scheduler.
  # The scheduler replies with a queue, from which the worker can pull jobs.
  # The "worker" object is used to collect metrics and perform admin.
  # The "capacity" gives the number of jobs that the worker will perform in
  # parallel. At present, this is only used to estimate the cluster's capacity;
  # workers can pull as many jobs as they like from the queue.
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
  name      @0 :Text;
  active    @1 :Bool;
  connected @2 :Bool;
}

# A callback for receiving progress updates.
interface Progress {
  report @0 (status :Text) -> ();
  # Called when the status changes.
}

interface PoolAdmin {
  show      @0 () -> (state :Text);
  workers   @1 () -> (workers : List(WorkerInfo));
  worker    @3 (worker :Text) -> (worker :Worker);

  setActive @2 (worker :Text, active :Bool, autoCreate :Bool) -> ();
  # Mark worker as active or paused.
  # This active flag is independent of the Queue.setActive one
  # (the worker is paused if either the admin or the worker sets it inactive).
  # This can be used even when the worker is disconnected.
  # If autoCreate is true then this can be used even with an unknown worker,
  # which may be useful if you want a new worker to start paused, for example.

  drain     @7 (worker :Text, progress :Progress) -> ();
  # Mark the worker as paused and wait until no jobs are running.
  # Returns immediately if the worker isn't connected.
  # If given, [progress] receives one-line progress reports.

  update    @4 (worker :Text, progress :Progress) -> ();
  # Drain worker, ask it to restart with the latest version, and return when it comes back.
  # If given, [progress] receives one-line progress reports.

  setRate   @5 (id :Text, rate :Float64) -> ();
  # Set the expected share of the pool for this client.

  forget    @6 (worker :Text) -> ();
  # Remove worker from the set of known workers.
}

interface Admin {
  pools        @0 () -> (names :List(Text));
  pool         @1 (name :Text) -> (pool :PoolAdmin);

  addClient    @2 (id :Text) -> (cap :Submission);
  # Return a new submission endpoint for client "id".
  # Returns an error if "id" is already registered.

  removeClient @3 (id :Text) -> (cap :Submission);
  # Remove a client, so that the sturdy-ref can no longer be used to connect.
  # The client cannot submit any more jobs after this, though existing jobs
  # are not cancelled.

  listClients  @4 () -> (clients :List(Text));
}

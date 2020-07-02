@0x8378b104cef5b2eb;

struct DockerBuild {
  dockerfile   @0 :Text;
  # The contents of the Dockerfile to build.

  pushTarget   @1 :Text;
  # If set, the builder will "docker push" to this target on success.
  # The format is "repo:tag". The tag is not optional.
  # You'll need to provide pushUser and pushPassword too when using this.
  # To avoid sharing important credentials, you can create a new Docker Hub
  # user for this and push to a staging repository. Then use the returned
  # RepoId hash to tag it in the final repository yourself.
  # Example value: "myorg/staging:job-123"

  pushUser     @2 :Text;
  pushPassword @3: Text;
}

struct JobDescr {
  dockerBuild @0 :DockerBuild;
  # Note: wrap this field in "action :union { }" to add more action types later.

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
  pop @0 (job :Job) -> (descr :JobDescr);
}

interface Worker {
  metrics @0 () -> (version :Text, data :Text);
  # Return the worker's Prometheus metrics.
}

interface Registration {
  register @0 (name :Text, worker :Worker) -> (queue :Queue);
}

interface Submission {
  submit @0 (pool :Text, descr :JobDescr, urgent :Bool) -> (job :Job);
}

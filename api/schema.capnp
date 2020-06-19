@0x8378b104cef5b2eb;

struct JobDescr {
  dockerfile @0 :Text;

  cacheHint @1 :Text;
  # Try to place jobs with the same cache_hint on the same node.
  # This will probably be a hash of the first few lines of the Dockerfile.
}

interface Job {
  log     @0 (start :Int64) -> (log :Data, next :Int64);
  # Return a chunk of log data starting at byte offset "start" and the
  # value to use for "start" in the next call to continue reading.
  # Returns 0 bytes of data to indicate end-of-file.
  # If the log is incomplete and there is no data currently available,
  # it waits until something changes before returning.
  # If "start" is negative then it is relative to the end of the log.

  status @1 () -> ();
  # Waits for the job to finish. Resolves to an error if the job fails.
}

interface Queue {
  pop @0 (job :Job) -> (descr :JobDescr);
}

interface Registration {
  register @0 (name :Text) -> (queue :Queue);
}

interface Submission {
  submit @0 (descr :JobDescr) -> (job :Job);
}

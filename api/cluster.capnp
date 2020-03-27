@0xea47cbb1dbdfe7d7;

interface Logger {
  log @0 (msg :Text) -> ();
  heartbeat @1 (msg :Text) -> (reply :Text);
}

interface Agent {
  exec @0 (cmd :Text) -> (exit_code :Int32);
}

interface Cluster {
  register @0 (hostname :Text, callback :Agent) -> (reply :Logger);
}


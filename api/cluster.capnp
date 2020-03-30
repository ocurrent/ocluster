@0xea47cbb1dbdfe7d7;

struct Command {
  binary @0 :Data;
  args @1 :List(Data);
}

interface ProcessOut {
  stdout @0 (chunk :Data) -> ();
  stderr @1 (chunk :Data) -> ();
  complete @2 (exitCode :Int32) -> ();
}

interface ProcessIn {
  stdin @0 (chunk :Data) -> ();
  cancel @1 () -> ();
}

interface Agent {
  exec @0 (cmd :Command) -> (exitCode :Int32);
  spawn @1 (cmd: Command, pout: ProcessOut) -> (pin: ProcessIn);
}

interface Cluster {
  register @0 (hostname :Text, callback :Agent) -> ();
  find @1 (hostname :Text) -> (callback :Agent);
  list @2 () -> (agents :List(Agent));
}
@0xea47cbb1dbdfe7d7;

struct Command {
  binary @0 :Data;
  args @1 :List(Data);
}

struct CommandResult {
  exitCode @0 :Int32;
  stdout @1 :Data;
  stderr @2 :Data;
}

interface ProcessOut {
  stdout @0 (chunk :Data) -> ();
  complete @1 (exitCode :Int32) -> ();
}

interface ProcessIn {
  stdin @0 (chunk :Data) -> ();
# close TODO
  cancel @1 () -> ();
}

interface Agent {
  exec @0 (cmd :Command) -> CommandResult;
  spawn @1 (cmd: Command, pout: ProcessOut) -> (pin: ProcessIn);
}

interface Cluster {
  register @0 (hostname :Text, callback :Agent) -> ();
  find @1 (hostname :Text) -> (callback :Agent);
  list @2 () -> (agents :List(Agent));
}

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

struct HostInfo {
   osDistrib @0 :Text;
   osVersion @1 :Text;
   arch @2 :Text;
}

interface Agent {
  exec @0 (cmd :Command) -> (exitCode :Int32);
  spawn @1 (cmd: Command, pout: ProcessOut) -> (pin: ProcessIn);
}

interface ClusterMember {
  register @0 (hostname :Text, hostinfo :HostInfo, callback :Agent) -> ();
}

interface ClusterUser {
  find @0 (hostname :Text) -> (callback :Agent);
  list @1 () -> (agents :List(Agent));
}

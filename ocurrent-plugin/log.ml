let src = Logs.Src.create "current_ocluster" ~doc:"OCurrent OCluster plugin"
include (val Logs.src_log src : Logs.LOG)

let src = Logs.Src.create "scheduler" ~doc:"ocluster-scheduler"
include (val Logs.src_log src : Logs.LOG)

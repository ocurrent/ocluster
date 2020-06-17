let src = Logs.Src.create "scheduler" ~doc:"build-scheduler"
include (val Logs.src_log src : Logs.LOG)

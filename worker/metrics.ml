open Prometheus

let namespace = "ocluster"
let subsystem = "worker"

let jobs_accepted =
  let help = "Number of jobs accepted in total" in
  Counter.v ~help ~namespace ~subsystem "jobs_accepted_total"

let job_time =
  let help = "Time jobs ran for" in
  Summary.v_label ~label_name:"result" ~help ~namespace ~subsystem "job_time_seconds"

let docker_push_time =
  let help = "Time uploading to Docker Hub" in
  Summary.v ~help ~namespace ~subsystem "docker_push_time_seconds"

let docker_prune_time =
  let help = "Time spent pruning Docker cache" in
  Summary.v ~help ~namespace ~subsystem "docker_prune_time_seconds"

let running_jobs =
  let help = "Number of jobs currently running" in
  Gauge.v ~help ~namespace ~subsystem "running_jobs"

let healthcheck_time =
  let help = "Time to perform last healthcheck" in
  Gauge.v ~help ~namespace ~subsystem "healthcheck_time_seconds"

let unhealthy =
  let help = "Number of unhealthy workers" in
  Gauge.v ~help ~namespace ~subsystem "unhealthy"

let cache_hits =
  let help = "Number of OBuilder cache hits" in
  Gauge.v ~help ~namespace ~subsystem "cache_hits"

let cache_misses =
  let help = "Number of OBuilder cache misses" in
  Gauge.v ~help ~namespace ~subsystem "cache_misses"

let obuilder_space_free =
  let help = "OBuilder percentage of space free" in
  Gauge.v ~help ~namespace ~subsystem "obuilder_space_free"


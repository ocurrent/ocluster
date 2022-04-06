let obuilder_spec_to_custom spec =
  let open Cluster_api.Raw in
  let custom = Builder.Custom.init_root () in
  let builder = Builder.Custom.payload_get custom in
  let obuilder = Builder.OBuilder.init_pointer builder in
  Builder.OBuilder.spec_set obuilder spec;
  let r = Reader.Custom.of_builder custom in
  Reader.Custom.payload_get r

let dockerfile_to_custom spec =
  let open Cluster_api.Raw in
  let custom = Builder.Custom.init_root () in
  let builder = Builder.Custom.payload_get custom in
  let df = Builder.DockerBuild.Dockerfile.init_pointer builder in
  Builder.DockerBuild.Dockerfile.contents_set df spec;
  let r = Reader.Custom.of_builder custom in
  Reader.Custom.payload_get r
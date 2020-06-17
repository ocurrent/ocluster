open Lwt.Infix

let ( >>!= ) = Lwt_result.bind

module Flow = struct
  type flow = {
    inbound : Cstruct.t Lwt_stream.t;
    outbound : Cstruct.t option -> unit;
    mutable closed : bool;
  }

  type error = |
  type write_error = Mirage_flow.write_error

  let pp_error _ = function (_ : error) -> .
  let pp_write_error = Mirage_flow.pp_write_error

  let pair () =
    let a, to_a = Lwt_stream.create () in
    let b, to_b = Lwt_stream.create () in
    { inbound = a; outbound = to_b; closed = false },
    { inbound = b; outbound = to_a; closed = false }

  let close t =
    assert (t.closed = false);
    t.closed <- true;
    t.outbound None;
    Lwt.return_unit

  let write t data =
    if t.closed then Lwt.return (Error `Closed)
    else (
      Lwt.pause () >>= fun () ->
      t.outbound (Some data);
      Lwt.return (Ok ())
    )

  let read t =
    assert (not t.closed);
    Lwt_stream.get t.inbound >|= function
    | None -> Ok `Eof
    | Some data -> Ok (`Data data)

  let rec writev t = function
    | [] -> Lwt_result.return ()
    | x :: xs ->
      write t x >>!= fun () ->
      writev t xs
end

module Net = Capnp_rpc_net.Networking(Capnp_rpc_net.Two_party_network)(Flow)

let id = Capnp_rpc_net.Restorer.Id.public "x"

(* Stretch our local reference to [x] over a fake network link. *)
let remote ~switch x =
  let local, remote = Flow.pair () in
  let peer_id = Capnp_rpc_net.Auth.Digest.insecure in
  let local_ep = Capnp_rpc_net.Endpoint.of_flow ~switch (module Flow) local ~peer_id in
  let remote_ep = Capnp_rpc_net.Endpoint.of_flow ~switch (module Flow) remote ~peer_id in
  let local_captp = Net.CapTP.connect local_ep ~restore:Capnp_rpc_net.Restorer.none in
  let _remote_captp = Net.CapTP.connect remote_ep ~restore:(Capnp_rpc_net.Restorer.single id x) in
  Net.CapTP.bootstrap local_captp id

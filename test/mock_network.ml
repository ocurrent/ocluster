open Capnp_rpc_lwt
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

  let ensure_closed t =
    if t.closed = false then (
      t.closed <- true;
      t.outbound None
    )

  let close t =
    assert (t.closed = false);
    ensure_closed t;
    Lwt.return_unit

  let pair ~switch () =
    let a, to_a = Lwt_stream.create () in
    let b, to_b = Lwt_stream.create () in
    let e1 = { inbound = a; outbound = to_b; closed = false } in
    let e2 = { inbound = b; outbound = to_a; closed = false } in
    Lwt_switch.add_hook (Some switch) (fun () ->
        ensure_closed e1;
        ensure_closed e2;
        Lwt.return_unit
      );
    e1, e2

  let write t data =
    Lwt.pause () >>= fun () ->
    if t.closed then Lwt.return (Error `Closed)
    else (
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

let sturdy (cap : 'a Capability.t) : 'a Sturdy_ref.t =
  Capnp_rpc_lwt.Cast.sturdy_of_raw @@ object
    method connect =
      Capability.inc_ref cap;
      Lwt_result.return (Capnp_rpc_lwt.Cast.cap_to_raw cap)

    method to_uri_with_secrets = Uri.of_string "mock sturdy-ref"
  end

(* Stretch our local reference to [x] over a fake network link. *)
let remote ~switch (x : 'a Capability.t) : 'a Sturdy_ref.t =
  let local, remote = Flow.pair ~switch () in
  let peer_id = Capnp_rpc_net.Auth.Digest.insecure in
  let local_ep = Capnp_rpc_net.Endpoint.of_flow ~switch (module Flow) local ~peer_id in
  let remote_ep = Capnp_rpc_net.Endpoint.of_flow ~switch (module Flow) remote ~peer_id in
  let local_captp = Net.CapTP.connect local_ep ~restore:Capnp_rpc_net.Restorer.none in
  let _remote_captp = Net.CapTP.connect remote_ep ~restore:(Capnp_rpc_net.Restorer.single id x) in
  let cap = lazy (Net.CapTP.bootstrap local_captp id) in
  Lwt_switch.add_hook (Some switch) (fun () -> Capability.dec_ref (Lazy.force cap); Lwt.return_unit);
  Capnp_rpc_lwt.Cast.sturdy_of_raw @@ object
    method connect =
      let cap = Lazy.force cap in
      Capability.inc_ref cap;
      Lwt_result.return (Capnp_rpc_lwt.Cast.cap_to_raw cap)

    method to_uri_with_secrets = Uri.of_string "mock sturdy-ref"
  end

(* [remote_breakable x] is a [(sr, break)] pair, where [sr] is a sturdy-ref that connects to [x] over a fake network.
   And [break] is a function that breaks the network. Connecting after breaking the network will succeed. *)
let remote_breakable x =
  let switch = ref (Lwt_switch.create ()) in
  let sr =
    Capnp_rpc_lwt.Cast.sturdy_of_raw @@ object
      method connect =
        Sturdy_ref.connect_exn (remote ~switch:!switch x) >>= fun cap ->
        Lwt_result.return (Capnp_rpc_lwt.Cast.cap_to_raw cap)

      method to_uri_with_secrets = Uri.of_string "breakable sturdy-ref"
    end
  in
  let break () =
    Lwt_switch.turn_off !switch >|= fun () ->
    switch := Lwt_switch.create ()
  in
  sr, break

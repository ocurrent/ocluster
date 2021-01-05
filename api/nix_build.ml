module Spec = struct
  type drv = [`Drv of string] [@@deriving to_yojson]
  type expr = [`Expr of string] [@@deriving to_yojson]
  
  type cmd = {
    drv : drv;
    exe : string;
    args: string list;
  } [@@deriving to_yojson]

  type t =
    | Eval of expr
    | Build of drv
    | Run of cmd
  [@@deriving to_yojson]

  let init b t =
    let module B = Raw.Builder.NixBuild in
    let module A = Raw.Builder.NixBuild.Action in
    let b = B.action_init b in
    begin match t with
      | Eval (`Expr expr) -> A.eval_set b expr
      | Build (`Drv drv) -> A.build_set b drv
      | Run { drv = `Drv drv; exe; args } ->
        let b = A.run_init b in
        let module B = Raw.Builder.NixCommand in
        let () = B.exe_set b exe in
        let _ = B.cmd_set_list b args in
        let _ = B.drv_set b drv in
        ()
    end
  
  let read r =
    let module R = Raw.Reader.NixBuild in
    let module RA = Raw.Reader.NixBuild.Action in
    match R.action_get r |> R.Action.get with
      | Eval expr -> Eval (`Expr expr)
      | Build drv -> Build (`Drv drv)
      | Run cmd ->
        let module RC = Raw.Reader.NixCommand in
        let drv = `Drv (RC.drv_get cmd) in
        let exe = RC.exe_get cmd in
        let args = RC.cmd_get_list cmd in
        Run { drv; exe; args }
      | Undefined x -> Fmt.failwith "Unknown action type %d" x

  let pp_drv f (`Drv drv) = Fmt.string f drv

  let pp f = function
    | Build drv -> pp_drv f drv
    | Run { drv; exe; args; } ->
      pp_drv f drv;
      Fmt.char f '/';
      Fmt.string f exe;
      Fmt.sp f ();
      Fmt.list ~sep:Fmt.sp Fmt.string f args
    | Eval (`Expr expr) -> Fmt.string f expr
end


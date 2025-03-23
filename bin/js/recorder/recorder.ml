open! Base
open Stdlib.Effect
open Stdlib.Effect.Deep
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open React_trace
open Lib_domains
open Concrete_domains
open Interp_effects
include Recorder_intf

let get_path_from_checkpoint = function
  | Retry_start (_, pt)
  | Render_check pt
  | Render_finish pt
  | Render_cancel pt
  | Effects_finish pt ->
      Some pt
  | Event _ -> None

type tree = { path : string; name : string; children : tree list }
and view = { msg : string; tree : tree }

and recording = {
  checkpoints : view list;
  root : (Concrete_domains.tree[@yojson.opaque]) option;
  log : string;
}
[@@deriving yojson_of]

let emp_recording = { checkpoints = []; root = None; log = "" }

let leaf : const -> tree = function
  | Unit -> { path = ""; name = "()"; children = [] }
  | Bool b -> { path = ""; name = Bool.to_string b; children = [] }
  | Int i -> { path = ""; name = Int.to_string i; children = [] }
  | String s -> { path = ""; name = s; children = [] }

let clos : clos -> tree = function
  | _ -> { path = ""; name = "button"; children = [] }

let rec tree : Concrete_domains.tree -> tree = function
  | T_const k -> leaf k
  | T_clos cl -> clos cl
  | T_list l -> list l
  | T_path p -> path p

and list (ts : Concrete_domains.tree list) : tree =
  { path = ""; name = "..."; children = List.map ts ~f:tree }

and path (pt : Path.t) : tree =
  let { comp_spec = { comp; _ }; children; _ } = perform (Lookup_view pt) in
  {
    path = pt |> Path.sexp_of_t |> Sexp.to_string;
    name = comp;
    children = [ tree children ];
  }

let event_h (type a b) (f : a -> b) (x : a) :
    recording:recording -> b * recording =
  match f x with
  | v -> fun ~recording -> (v, recording)
  | effect Tree_update_st (path, label, (v, q)), k ->
      fun ~recording ->
        let () = perform (Tree_update_st (path, label, (v, q))) in
        let recording =
          {
            recording with
            log =
              recording.log
              ^ Printf.sprintf "[path %s] Update state %s -> %s\n"
                  (Sexp.to_string (Path.sexp_of_t path))
                  label
                  (Sexp.to_string (sexp_of_value v));
          }
        in
        continue k () ~recording
  | effect Tree_set_dec (path, dec), k ->
      fun ~recording ->
        let () = perform (Tree_set_dec (path, dec)) in
        let recording =
          {
            recording with
            log =
              recording.log
              ^ Printf.sprintf "[path %s] Set decision %s\n"
                  (Sexp.to_string (Path.sexp_of_t path))
                  (Sexp.to_string (sexp_of_decision dec));
          }
        in
        continue k () ~recording
  | effect Alloc_pt, k ->
      fun ~recording ->
        let path = perform Alloc_pt in
        let recording =
          {
            recording with
            log =
              recording.log
              ^ Printf.sprintf "Allocate path %s\n"
                  (Sexp.to_string (Path.sexp_of_t path));
          }
        in
        continue k path ~recording
  | effect Checkpoint { msg; checkpoint }, k ->
      fun ~recording ->
        let pt = get_path_from_checkpoint checkpoint in
        let msg =
          Printf.sprintf "[%s] %s"
            (Sexp.to_string ([%sexp_of: Path.t option] pt))
            msg
        in
        let root = perform Get_root in
        let tree = tree root in
        let recording =
          {
            recording with
            checkpoints = { msg; tree } :: recording.checkpoints;
          }
        in
        continue k () ~recording

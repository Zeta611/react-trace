open! Base
open Stdlib.Effect
open Stdlib.Effect.Deep
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open React_trace
open Lib_domains
open Concrete_domains
open Interp_effects
include Recorder_intf

type source_loc = {
  start_line : int;
  start_col : int;
  end_line : int;
  end_col : int;
}
[@@deriving yojson_of]

let source_loc_of_location (loc : Location.t) : source_loc =
  let open Lexing in
  {
    start_line = loc.loc_start.pos_lnum;
    start_col = loc.loc_start.pos_cnum - loc.loc_start.pos_bol;
    end_line = loc.loc_end.pos_lnum;
    end_col = loc.loc_end.pos_cnum - loc.loc_end.pos_bol;
  }

type state_entry = { label : string; value : string; queue_size : int }
[@@deriving yojson_of]

type decision_info = { chk : bool; eff : bool } [@@deriving yojson_of]

type tree = {
  path : string;
  name : string;
  children : tree list;
  st_store : state_entry list option;
  eff_q_size : int option;
  dec : decision_info option;
  arg : string option;
}
[@@deriving yojson_of]

type view = { msg : string; tree : tree; source_loc : source_loc option }
[@@deriving yojson_of]

type recording = {
  checkpoints : view list;
  root : (Concrete_domains.tree[@yojson.opaque]) option;
  log : string;
}
[@@deriving yojson_of]

let emp_recording = { checkpoints = []; root = None; log = "" }

let leaf : const -> tree = function
  | Unit ->
      {
        path = "";
        name = "()";
        children = [];
        st_store = None;
        eff_q_size = None;
        dec = None;
        arg = None;
      }
  | Bool b ->
      {
        path = "";
        name = Bool.to_string b;
        children = [];
        st_store = None;
        eff_q_size = None;
        dec = None;
        arg = None;
      }
  | Int i ->
      {
        path = "";
        name = Int.to_string i;
        children = [];
        st_store = None;
        eff_q_size = None;
        dec = None;
        arg = None;
      }
  | String s ->
      {
        path = "";
        name = s;
        children = [];
        st_store = None;
        eff_q_size = None;
        dec = None;
        arg = None;
      }

let clos : clos -> tree = function
  | _ ->
      {
        path = "";
        name = "button";
        children = [];
        st_store = None;
        eff_q_size = None;
        dec = None;
        arg = None;
      }

let rec tree : Concrete_domains.tree -> tree = function
  | T_const k -> leaf k
  | T_clos cl -> clos cl
  | T_list l -> list l
  | T_path p -> path p

and list (ts : Concrete_domains.tree list) : tree =
  {
    path = "";
    name = "...";
    children = List.map ts ~f:tree;
    st_store = None;
    eff_q_size = None;
    dec = None;
    arg = None;
  }

and path (pt : Path.t) : tree =
  let { comp_spec = { comp; arg }; dec; st_store; eff_q; children } =
    perform (Lookup_view pt)
  in
  (* Convert st_store to state_entry list *)
  let st_store_entries =
    St_store.to_alist st_store
    |> List.map ~f:(fun (lbl, (v, q)) ->
           {
             label = lbl;
             value = Sexp.to_string (sexp_of_value v);
             queue_size = Job_q.size q;
           })
  in
  (* Convert decision *)
  let dec_info = { chk = dec.chk; eff = dec.eff } in
  (* Convert arg to string *)
  let arg_str = Sexp.to_string (sexp_of_value arg) in
  {
    path = pt |> Path.sexp_of_t |> Sexp.to_string;
    name = comp;
    children = [ tree children ];
    st_store = Some st_store_entries;
    eff_q_size = Some (Job_q.size eff_q);
    dec = Some dec_info;
    arg = Some arg_str;
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
  | effect Set_root t, k ->
      fun ~recording ->
        continue k () ~recording:{ recording with root = Some t }
  | effect Checkpoint { msg; component_info; checkpoint; loc }, k ->
      fun ~recording ->
        (* Format message based on checkpoint type *)
        let formatted_msg =
          match (checkpoint, component_info) with
          | Event i, _ -> Printf.sprintf ":event: Event %d triggered: %s" i msg
          | Retry_start (attempt, _), Some (name, _dec) ->
              Printf.sprintf ":retry: <%s> %s (attempt %d)" name msg attempt
          | Render_check _, Some (name, _dec) ->
              Printf.sprintf ":check: <%s> %s" name msg
          | Render_finish _, Some (name, _dec) ->
              Printf.sprintf ":finish: <%s> %s" name msg
          | Render_cancel _, Some (name, _dec) ->
              Printf.sprintf ":cancel: <%s> %s" name msg
          | Effects_finish _, Some (name, _dec) ->
              Printf.sprintf ":effects: <%s> %s" name msg
          | Hook_eval kind, _ ->
              let kind_str =
                match kind with
                | Use_state -> "useState"
                | Use_effect -> "useEffect"
                | Setter_call -> "setter"
              in
              Printf.sprintf ":hook: [%s] %s" kind_str msg
          | _, None ->
              (* This shouldn't happen unless we missed a component name
                 somewhere *)
              Printf.sprintf ":default: %s" msg
        in
        let root = Option.value ~default:(T_const Unit) recording.root in
        let tree = tree root in
        let source_loc = Option.map loc ~f:source_loc_of_location in
        let recording =
          {
            recording with
            checkpoints =
              { msg = formatted_msg; tree; source_loc } :: recording.checkpoints;
          }
        in
        continue k () ~recording

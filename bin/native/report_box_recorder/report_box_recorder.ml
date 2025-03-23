open! Base
open Stdlib.Effect
open Stdlib.Effect.Deep
open React_trace
open Interp_effects
open Lib_domains
open Concrete_domains
include Recorder_intf
module B = PrintBox

type recording = (string * B.t) list * tree option

let align ?(h = `Center) ?(v = `Center) = B.align ~h ~v
let bold_text = B.(text_with_style Style.bold)

let trunc ?(max_len = 10) s =
  if String.length s > max_len then String.prefix s max_len ^ "…" else s

let value (v : value) : B.t =
  sexp_of_value v |> Sexp.to_string |> trunc |> B.text

let clos ({ param; _ } : clos) : B.t = "λ" ^ param ^ ".<body>" |> B.text

let leaf : const -> B.t = function
  | Unit -> B.text "()"
  | Bool b -> B.bool b
  | Int i -> B.int i
  | String s -> B.text s

let rec tree : tree -> B.t = function
  | T_const k -> leaf k
  | T_clos cl -> clos cl
  | T_list l -> list l
  | T_path p -> path p

and list (ts : tree list) : B.t = B.hlist_map (fun t -> tree t |> align) ts

and path (pt : Path.t) : B.t =
  let { comp_spec = { comp; arg; _ }; dec; st_store; eff_q; children } =
    perform (Lookup_view pt)
  in
  let comp_spec_box =
    B.(hlist ~bars:false [ bold_text (trunc comp); text " "; value arg ])
    |> align
  in
  let dec_box =
    let dec = sexp_of_decision dec |> Sexp.to_string in
    B.(hlist_map text [ "dec"; dec ])
  in
  let stt_box =
    let st_trees =
      let st_store = St_store.to_alist st_store in
      List.map st_store ~f:(fun (lbl, (value, job_q)) ->
          let value = Sexp.to_string (sexp_of_value value) in
          let job_q = Job_q.to_list job_q |> List.map ~f:clos in

          B.(tree (text ("Stt" ^ lbl ^ " ↦ " ^ value)) job_q))
      |> B.vlist
    in
    B.(hlist [ text "stt"; st_trees ])
  in
  let eff_box =
    let eff_q = Job_q.to_list eff_q |> List.map ~f:clos in
    B.(hlist [ text "eff"; vlist eff_q ])
  in
  let part_view_box = B.vlist [ comp_spec_box; dec_box; stt_box; eff_box ] in
  let children = tree children in
  B.(vlist [ part_view_box; children ] |> frame)

let emp_recording = ([], None)

let event_h (type a b) (f : a -> b) (x : a) :
    recording:recording -> b * recording =
  match f x with
  | v -> fun ~recording -> (v, recording)
  | effect Checkpoint { msg; _ }, k -> (
      fun ~recording ->
        try
          let root = Option.value_exn (snd recording) in
          let box = tree root in
          continue k () ~recording:((msg, box) :: fst recording, Some root)
        with _ -> continue k () ~recording)
  | effect Set_root t, k ->
      fun ~recording -> continue k () ~recording:(fst recording, Some t)

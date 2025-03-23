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
  | effect Checkpoint { msg; component_info; checkpoint }, k -> (
      fun ~recording ->
        try
          let root = Option.value_exn (snd recording) in
          let box = tree root in

          (* Format message based on checkpoint type for the box *)
          let title =
            match (checkpoint, component_info) with
            | Event i, _ ->
                B.(
                  vlist
                    [
                      text_with_style Style.bold (Printf.sprintf "⚡ Event %d" i);
                      text msg;
                    ])
            | Retry_start (attempt, _), Some (name, dec) ->
                B.(
                  vlist
                    [
                      text_with_style Style.bold
                        (Printf.sprintf "🔁 %s (chk:%b,eff:%b) (attempt %d)" name
                           dec.chk dec.eff attempt);
                      text msg;
                    ])
            | Render_check _, Some (name, dec) ->
                B.(
                  vlist
                    [
                      text_with_style Style.bold
                        (Printf.sprintf "🏗️ %s (chk:%b,eff:%b)" name dec.chk
                           dec.eff);
                      text msg;
                    ])
            | Render_finish _, Some (name, dec) ->
                B.(
                  vlist
                    [
                      text_with_style Style.bold
                        (Printf.sprintf "✅ %s (chk:%b,eff:%b)" name dec.chk
                           dec.eff);
                      text msg;
                    ])
            | Render_cancel _, Some (name, dec) ->
                B.(
                  vlist
                    [
                      text_with_style Style.bold
                        (Printf.sprintf "⏩ %s (chk:%b,eff:%b)" name dec.chk
                           dec.eff);
                      text msg;
                    ])
            | Effects_finish _, Some (name, dec) ->
                B.(
                  vlist
                    [
                      text_with_style Style.bold
                        (Printf.sprintf "⚙️ %s (chk:%b,eff:%b)" name dec.chk
                           dec.eff);
                      text msg;
                    ])
            | _, None -> B.text msg
          in

          (* Add the title to the box *)
          let titled_box = B.(vlist [ title; box ]) in

          (* Create a formatted message for the recording list *)
          let formatted_msg =
            match (checkpoint, component_info) with
            | Event i, _ -> Printf.sprintf "Event %d: %s" i msg
            | _, Some (name, dec) ->
                Printf.sprintf "%s (chk:%b,eff:%b): %s" name dec.chk dec.eff msg
            | _, None -> msg
          in

          continue k ()
            ~recording:((formatted_msg, titled_box) :: fst recording, Some root)
        with _ -> continue k () ~recording)
  | effect Set_root t, k ->
      fun ~recording -> continue k () ~recording:(fst recording, Some t)

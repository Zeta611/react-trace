open Base
open Stdlib.Effect.Deep
open Interp_effects
include Recorder_intf

type recording = Lib_domains.Concrete_domains.tree option

let emp_recording : recording = None

let event_h (type a b) (f : a -> b) (x : a) :
    recording:recording -> b * recording =
  match f x with
  | v -> fun ~recording -> (v, recording)
  | effect Checkpoint _, k -> fun ~recording -> continue k () ~recording
  | effect Set_root t, k ->
      fun ~recording:_ -> continue k () ~recording:(Some t)
  | effect Get_root, k ->
      fun ~recording -> continue k (Option.value_exn recording) ~recording

open Stdlib.Effect.Deep
open Interp_effects
include Recorder_intf

type recording = unit

let emp_recording = ()

let event_h (type a b) (f : a -> b) (x : a) :
    recording:recording -> b * recording =
  match f x with
  | v -> fun ~recording -> (v, recording)
  | effect Checkpoint _, k -> fun ~recording -> continue k () ~recording

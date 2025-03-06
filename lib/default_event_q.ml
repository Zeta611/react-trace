open Stdlib.Effect.Deep
open Interp_effects

(* push finite events *)
let event_h (type a b) (f : a -> b) (x : a) : event_q:int list -> b =
  match f x with
  | v ->
      fun ~event_q ->
        Logger.event event_q `Ret;
        v
  | effect Listen, k -> (
      fun ~event_q ->
        Logger.event event_q `Listen;
        match event_q with
        | [] -> continue k ~event_q:[] None
        | e :: event_q -> continue k ~event_q (Some e))

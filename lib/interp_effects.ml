open! Base
open Lib_domains
open Syntax
open Concrete_domains

exception Unbound_var of string
exception Type_error
exception Invalid_phase
exception Unreachable

(* path and phase effects *)
type _ eff += Rd_pt : Path.t eff | Rd_ph : phase eff

(* environmental effects *)
type _ eff +=
  | Rd_env : Env.t eff
  | In_env : Env.t -> (('b -> 'a) -> 'b -> 'a) eff

(* memory effects *)
type _ eff +=
  | Alloc_addr : obj -> Addr.t eff
  | Lookup_addr : Addr.t -> obj eff
  | Update_addr : Addr.t * obj -> unit eff

(* view effects in eval/eval_mult *)
type _ eff +=
  | View_lookup_st : Label.t -> (value * Job_q.t) eff
  | View_update_st : (Label.t * (value * Job_q.t)) -> unit eff
  | View_get_dec : decision eff
  | View_add_dec : decision -> unit eff
  | View_set_dec : decision -> unit eff
  | View_enq_eff : clos -> unit eff
  | View_flush_eff : unit eff
  | View_get_comp_name : Id.t eff

(* tree memory effects in eval/eval_mult *)
type _ eff +=
  | Tree_lookup_st : Path.t * Label.t -> (value * Job_q.t) eff
  | Tree_update_st : (Path.t * Label.t * (value * Job_q.t)) -> unit eff
  | Tree_get_dec : Path.t -> decision eff
  | Tree_add_dec : Path.t * decision -> unit eff
  | Tree_set_dec : Path.t * decision -> unit eff
  | Tree_flush_eff : Path.t -> unit eff
  | Tree_get_comp_name : Path.t -> Id.t eff

(* tree memory effects in render *)
type _ eff +=
  | Alloc_pt : Path.t eff
  | Lookup_view : Path.t -> view eff
  | Update_view : Path.t * view -> unit eff

(* component definition table effects *)
type _ eff += Lookup_comp : Id.t -> comp_def eff | Get_comp_env : Env.t eff

(* I/O effects *)
type _ eff += Print : string -> unit eff

(* event queue effects *)
type _ eff += Listen : int option eff

(* tree memory effects for instrumentation *)
type _ eff += Set_root : tree -> unit eff

type checkpoint =
  | Retry_start of (int * Path.t)
  | Render_check of Path.t
  | Render_finish of Path.t
  | Render_cancel of Path.t
  | Effects_finish of Path.t
  | Event of int

type _ eff +=
  | Checkpoint : {
      msg : string;
      component_info : (string * decision) option;
      checkpoint : checkpoint;
    }
      -> unit eff

(* For testing nontermination *)
type _ eff += Re_render_limit : int eff

exception Too_many_re_renders

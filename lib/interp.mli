(** This module defines the React-tRace definitional interpreter. It provides
    functions and handlers to evaluate expressions, manage memory, and handle
    various side-effects of the interpreter's components. *)

open! Base
open Lib_domains
open Syntax
open Concrete_domains

(** {1 Effect Handlers}

    These functions handle various computational effects used throughout the
    interpreter. Each handler manages a specific aspect of the interpreter's
    execution context. *)

val re_render_limit_h : ('a -> 'b) -> 'a -> re_render_limit:int -> 'b
(** [re_render_limit_h f x ~re_render_limit] handles the re-render limit effect,
    providing a maximum number of re-renders before termination to prevent
    infinite loops. *)

val ph_h : ('a -> 'b) -> 'a -> ph:phase -> 'b
(** [ph_h f x ~ph] handles phase effects, managing the current execution phase
    (Init, Succ, or Normal) during component evaluation. *)

val env_h : ('a -> 'b) -> 'a -> env:Env.t -> 'b
(** [env_h f x ~env] handles environment effects, managing variable bindings and
    scoping during expression evaluation. *)

val mem_h : ('a -> 'b) -> 'a -> mem:Memory.t -> 'b * Memory.t
(** [mem_h f x ~mem] handles memory effects for object allocation and access.
    Returns both the result and the updated memory state. *)

val view_h : ('a -> 'b) -> 'a -> view:view -> 'b * view
(** [view_h f x ~view] handles view-related effects, managing component state,
    decisions, and effect queues. Returns both the result and updated view. *)

val treemem_h : ('a -> 'b) -> 'a -> treemem:Tree_mem.t -> 'b * Tree_mem.t
(** [treemem_h f x ~treemem] handles tree memory effects, managing the global
    component tree structure and state. Returns both the result and updated tree
    memory. *)

val deftab_h : ('a -> 'b) -> 'a -> deftab:Def_tab.t -> 'b
(** [deftab_h f x ~deftab] handles definition table effects, providing access to
    component definitions during evaluation. *)

val io_h : ('a -> 'b) -> 'a -> output:string -> 'b * string
(** [io_h f x ~output] handles I/O effects, accumulating console output during
    program execution. Returns both the result and accumulated output. *)

(** {1 Core Evaluation Functions} *)

val eval : 'a Expr.t -> value
(** [eval expr] evaluates a React-tRace expression to a value, handling all
    language constructs including hooks, function calls, and state updates. *)

val eval_mult : ?re_render:int -> 'a Expr.t -> value
(** [eval_mult ?re_render expr] evaluates an expression with retry logic,
    re-evaluating when state changes occur during evaluation. The optional
    [re_render] parameter tracks the current retry count. *)

(** {1 Rendering Pipeline Functions} *)

val init : view_spec -> tree
(** [init vs] initializes a view specification into a concrete tree structure,
    setting up component instances and their initial state. *)

val reconcile : tree -> view_spec -> tree
(** [reconcile old_tree vs] reconciles an existing tree with a new view
    specification, determining what parts need to be updated, preserved, or
    recreated. *)

val check : tree -> bool
(** [check tree] checks whether any component in the tree requires re-rendering
    due to state changes. Returns [true] if re-rendering occurred. *)

val commit_effs : tree -> unit
(** [commit_effs tree] commits all queued effects in the tree, executing them in
    the correct order (depth-first, post-order). *)

(** {1 Program Analysis Functions} *)

val top_exp : Prog.t -> Expr.hook_free_t
(** [top_exp prog] extracts the top-level expression from a program, stripping
    away component definitions. *)

val collect : Prog.t -> Def_tab.t
(** [collect prog] collects all component definitions from a program into a
    definition table for lookup during evaluation. *)

val handlers : tree -> (int, clos) List.Assoc.t
(** [handlers tree] extracts all event handler closures from a tree with their
    stable indices, in the order they appear. *)

val step_loop : int -> tree -> unit
(** [step_loop i tree] executes the [i]-th event handler found in the tree,
    simulating user interaction with the interface. *)

(** {1 Main Execution Interface} *)

type 'recording run_info = {
  steps : int;  (** Number of render steps executed *)
  mem : Memory.t;  (** Final memory state *)
  treemem : Tree_mem.t;  (** Final tree memory state *)
  output : string;  (** Accumulated console output *)
  recording : 'recording;  (** Recorder-specific data *)
}
(** Result type containing all outputs from running the interpreter. *)

val run :
  ?fuel:int ->
  event_q_handler:
    ((unit -> (((int * 'recording) * Tree_mem.t) * Memory.t) * string) ->
    unit ->
    (((int * 'recording) * Tree_mem.t) * Memory.t) * string) ->
  recorder:(module Recorder_intf.Intf with type recording = 'recording) ->
  Prog.t ->
  'recording run_info
(** [run ?fuel ~event_q_handler ~recorder prog] executes a React-tRace program
    with the given configuration.

    @param fuel
      Optional limit on number of render cycles to prevent infinite loops
    @param event_q_handler Function to handle event queue processing
    @param recorder Module for recording execution traces
    @param prog The program to execute

    @return A [run_info] record containing all execution results *)

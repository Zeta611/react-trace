open! Base
open Stdlib.Effect
open Stdlib.Effect.Deep
open Lib_domains
open Syntax
open Concrete_domains
open Interp_effects

let re_render_limit_h (type a b) (f : a -> b) (x : a) : re_render_limit:int -> b
    =
  match f x with
  | v -> fun ~re_render_limit:_ -> v
  | effect Re_render_limit, k ->
      fun ~re_render_limit -> continue k re_render_limit ~re_render_limit

let ptph_h (type a b) (f : a -> b) (x : a) : ptph:Path.t * phase -> b =
  match f x with
  | v ->
      fun ~ptph ->
        Logger.ptph ptph `Ret;
        v
  | effect Rd_pt, k ->
      fun ~ptph ->
        Logger.ptph ptph `Rd_pt;
        continue k (fst ptph) ~ptph
  | effect Rd_ph, k ->
      fun ~ptph ->
        Logger.ptph ptph `Rd_ph;
        continue k (snd ptph) ~ptph

let rec env_h : type a b. (a -> b) -> a -> env:Env.t -> b =
 fun f x ->
  match f x with
  | v ->
      fun ~env ->
        Logger.env env `Ret;
        v
  | effect Rd_env, k ->
      fun ~env ->
        Logger.env env `Rd_env;
        continue k env ~env
  | effect In_env env', k ->
      fun ~env ->
        Logger.env env (`In_env env');
        continue k (env_h ~env:env') ~env

let mem_h (type a b) (f : a -> b) (x : a) : mem:Memory.t -> b * Memory.t =
  match f x with
  | v ->
      fun ~mem ->
        Logger.mem mem `Ret;
        (v, mem)
  | effect Alloc_addr obj, k ->
      fun ~mem ->
        Logger.mem mem `Alloc_addr;
        let addr = Memory.alloc mem in
        let mem = Memory.update mem ~addr ~obj in
        continue k addr ~mem
  | effect Lookup_addr addr, k ->
      fun ~mem ->
        Logger.mem mem (`Lookup_addr addr);
        continue k (Memory.lookup mem ~addr) ~mem
  | effect Update_addr (addr, v), k ->
      fun ~mem ->
        Logger.mem mem (`Update_addr (addr, v));
        continue k () ~mem:(Memory.update mem ~addr ~obj:v)

let treemem_h (type a b) (f : a -> b) (x : a) :
    treemem:Tree_mem.t -> b * Tree_mem.t =
  match f x with
  | v ->
      fun ~treemem ->
        Logger.treemem treemem `Ret;
        (v, treemem)
  | effect Lookup_st (path, label), k ->
      fun ~treemem ->
        Logger.treemem treemem (`Lookup_st (path, label));
        continue k (Tree_mem.lookup_st treemem ~path ~label) ~treemem
  | effect Update_st (path, label, (v, q)), k ->
      fun ~treemem ->
        Logger.treemem treemem (`Update_st (path, label, (v, q)));
        continue k () ~treemem:(Tree_mem.update_st treemem ~path ~label (v, q))
  | effect Get_dec path, k ->
      fun ~treemem ->
        Logger.treemem treemem (`Get_dec path);
        continue k (Tree_mem.get_dec treemem ~path) ~treemem
  | effect Set_dec (path, dec), k ->
      fun ~treemem ->
        Logger.treemem treemem (`Set_dec (path, dec));
        continue k () ~treemem:(Tree_mem.set_dec treemem ~path dec)
  | effect Set_arg (path, arg), k ->
      fun ~treemem ->
        Logger.treemem treemem (`Set_arg (path, arg));
        continue k () ~treemem:(Tree_mem.set_arg treemem ~path arg)
  | effect Enq_eff (path, clos), k ->
      fun ~treemem ->
        Logger.treemem treemem (`Enq_eff (path, clos));
        continue k () ~treemem:(Tree_mem.enq_eff treemem ~path clos)
  (* NOTE: in render *)
  | effect Alloc_pt, k ->
      fun ~treemem ->
        Logger.treemem treemem `Alloc_pt;
        continue k (Tree_mem.alloc_pt treemem) ~treemem
  | effect Lookup_ent path, k ->
      fun ~treemem ->
        Logger.treemem treemem (`Lookup_ent path);
        continue k (Tree_mem.lookup_ent treemem ~path) ~treemem
  | effect Update_ent (path, ent), k ->
      fun ~treemem ->
        Logger.treemem treemem (`Update_ent (path, ent));
        continue k () ~treemem:(Tree_mem.update_ent treemem ~path ent)
  (* NOTE: instrumentation *)
  | effect Get_root_pt, k ->
      fun ~treemem -> continue k (Tree_mem.root_pt treemem) ~treemem

let deftab_h (type a b) (f : a -> b) (x : a) : deftab:Def_tab.t -> b =
  match f x with
  | v ->
      fun ~deftab ->
        Logger.deftab deftab `Ret;
        v
  | effect Lookup_comp comp, k ->
      fun ~deftab ->
        Logger.deftab deftab (`Lookup_comp comp);
        continue k (Def_tab.lookup deftab ~comp) ~deftab

let io_h (type a b) (f : a -> b) (x : a) : output:string -> b * string =
  match f x with
  | v ->
      fun ~output ->
        Logger.io output `Ret;
        (v, output)
  | effect Print s, k ->
      fun ~output ->
        Logger.io output (`Print s);
        continue k () ~output:(output ^ s ^ "\n")

let value_exn exn v =
  Option.value_exn v ~error:(Error.of_exn exn ~backtrace:`Get)

let int_of_value_exn v = v |> Value.to_int |> value_exn Type_error
let bool_of_value_exn v = v |> Value.to_bool |> value_exn Type_error
let string_of_value_exn v = v |> Value.to_string |> value_exn Type_error
let addr_of_value_exn v = v |> Value.to_addr |> value_exn Type_error
let vs_of_value_exn v = v |> Value.to_vs |> value_exn Type_error
let clos_of_value_exn v = v |> Value.to_clos |> value_exn Type_error

let build_app_env (clos_value : clos) (arg : value) : Env.t =
  (* The function name is bound before the parameters, according to js semantics
     (ECMA-262 14th edition, p.347, "15.2.5 Runtime Semantics:
     InstantiateOrdinaryFunctionExpression": "5. Perform !
     funcEnv.CreateImmutableBinding(name, false)."). *)
  let { self; param; body = _; env } = clos_value in
  let env =
    match self with
    | None -> env
    | Some self -> Env.extend env ~id:self ~value:(Clos clos_value)
  in
  let env = Env.extend env ~id:param ~value:arg in
  env

let rec eval : type a. a Expr.t -> value =
 fun expr ->
  Logger.eval expr;
  match expr.desc with
  | Const Unit -> Const Unit
  | Const (Bool b) -> Const (Bool b)
  | Const (Int i) -> Const (Int i)
  | Const (String s) -> Const (String s)
  | Var id ->
      let env = perform Rd_env in
      Env.lookup env ~id
  | Comp c -> Comp c
  | View es ->
      View_spec (Vs_list (List.map es ~f:(fun e -> eval e |> vs_of_value_exn)))
  | Cond { pred; con; alt } ->
      let p = eval pred |> bool_of_value_exn in
      if p then eval con else eval alt
  | Fn { self; param; body } -> Clos { self; param; body; env = perform Rd_env }
  | App { fn; arg } -> (
      match eval fn with
      | Clos ({ body; _ } as clos_value) ->
          let env = build_app_env clos_value (eval arg) in
          perform (In_env env) eval body
      | Comp comp -> Comp_spec { comp; arg = eval arg }
      | Set_clos { label; path } ->
          (* Argument to the setter should be a setting thunk *)
          let clos = eval arg |> clos_of_value_exn in

          let self_pt = perform Rd_pt in
          let phase = perform Rd_ph in

          let dec =
            if Path.(path = self_pt) && Phase.(phase <> P_effect) then Retry
            else Update
          in
          perform (Set_dec (path, dec));

          (*if Int.(path = self_pt) && Phase.(phase <> P_effect) then*)
          (*  perform (Set_dec (path, Retry));*)
          let v, q = perform (Lookup_st (path, label)) in
          perform (Update_st (path, label, (v, Job_q.enqueue q clos)));

          Const Unit
      | _ -> raise Type_error)
  | Let { id; bound; body } ->
      let value = eval bound in
      let env = Env.extend (perform Rd_env) ~id ~value in
      perform (In_env env) eval body
  | Stt { label; stt; set; init; body } -> (
      let path = perform Rd_pt in
      match perform Rd_ph with
      | P_init ->
          let v = eval init in
          let env =
            perform Rd_env
            |> Env.extend ~id:stt ~value:v
            |> Env.extend ~id:set ~value:(Set_clos { label; path })
          in
          perform (Update_st (path, label, (v, Job_q.empty)));
          perform (In_env env) eval body
      | P_succ ->
          let v_old, q = perform (Lookup_st (path, label)) in
          (* Run the setting thunks in the set queue *)
          let v =
            Job_q.fold q ~init:v_old ~f:(fun value clos ->
                let { body; _ } = clos in
                let env = build_app_env clos value in
                perform (In_env env) eval body)
          in

          let env =
            perform Rd_env
            |> Env.extend ~id:stt ~value:v
            |> Env.extend ~id:set ~value:(Set_clos { label; path })
          in
          if Value.(v_old <> v) then perform (Set_dec (path, Update));
          perform (Update_st (path, label, (v, Job_q.empty)));
          perform (In_env env) eval body
      | P_effect -> raise Invalid_phase)
  | Eff e ->
      let path = perform Rd_pt
      and phase = perform Rd_ph
      and env = perform Rd_env in
      (match phase with P_effect -> raise Invalid_phase | _ -> ());
      perform (Enq_eff (path, { self = None; param = Id.unit; body = e; env }));
      Const Unit
  | Seq (e1, e2) ->
      eval e1 |> ignore;
      eval e2
  | Uop { op; arg } ->
      let v = eval arg in
      let k =
        match (op, v) with
        | Not, Const (Bool b) -> Bool (not b)
        | Uplus, Const (Int i) -> Int i
        | Uminus, Const (Int i) -> Int ~-i
        | _, _ -> raise Type_error
      in
      Const k
  | Bop { op; left; right } ->
      let v1 = eval left in
      let v2 = eval right in
      let k =
        match (op, v1, v2) with
        | Eq, Const Unit, Const Unit -> Bool true
        | Eq, Const (Bool b1), Const (Bool b2) -> Bool Bool.(b1 = b2)
        | Eq, Const (Int i1), Const (Int i2) -> Bool (i1 = i2)
        | Lt, Const (Int i1), Const (Int i2) -> Bool (i1 < i2)
        | Gt, Const (Int i1), Const (Int i2) -> Bool (i1 > i2)
        | Le, Const (Int i1), Const (Int i2) -> Bool (i1 <= i2)
        | Ge, Const (Int i1), Const (Int i2) -> Bool (i1 >= i2)
        | Ne, Const Unit, Const Unit -> Bool false
        | Ne, Const (Bool b1), Const (Bool b2) -> Bool Bool.(b1 <> b2)
        | Ne, Const (Int i1), Const (Int i2) -> Bool (i1 <> i2)
        | And, Const (Bool b1), Const (Bool b2) -> Bool (b1 && b2)
        | Or, Const (Bool b1), Const (Bool b2) -> Bool (b1 || b2)
        | Plus, Const (Int i1), Const (Int i2) -> Int (i1 + i2)
        | Minus, Const (Int i1), Const (Int i2) -> Int (i1 - i2)
        | Times, Const (Int i1), Const (Int i2) -> Int (i1 * i2)
        | _, _, _ -> raise Type_error
      in
      Const k
  | Alloc ->
      let addr = perform (Alloc_addr Obj.empty) in
      Addr addr
  | Get { obj; idx } ->
      let addr = eval obj |> addr_of_value_exn in
      let i = eval idx |> string_of_value_exn in
      let obj = perform (Lookup_addr addr) in
      Obj.lookup obj ~field:i
  | Set { obj; idx; value } ->
      let addr = eval obj |> addr_of_value_exn in
      let i = eval idx |> string_of_value_exn in
      let old_obj = perform (Lookup_addr addr) in
      let value = eval value in
      let new_obj = Obj.update old_obj ~field:i ~value in
      perform (Update_addr (addr, new_obj));
      Const Unit
  | Print s ->
      perform (Print (string_of_value_exn (eval s)));
      Const Unit

let rec eval_mult : type a. ?re_render:int -> a Expr.t -> value =
 fun ?(re_render = 1) expr ->
  Logger.eval_mult expr;

  (* This is a hack only used for testing non-termination. *)
  (try if re_render >= perform Re_render_limit then raise Too_many_re_renders
   with Stdlib.Effect.Unhandled Re_render_limit -> ());

  let v = eval expr in
  let path = perform Rd_pt in
  match perform (Get_dec path) with
  | Retry ->
      let re_render = re_render + 1 in
      perform
        (Checkpoint
           { msg = "Will retry"; checkpoint = Retry_start (re_render, path) });
      let ent = perform (Lookup_ent path) in
      perform (Update_ent (path, { ent with eff_q = Job_q.empty }));
      ptph_h ~ptph:(path, P_succ) (eval_mult ~re_render) expr
  | Idle | Update -> v

let rec render (vs : view_spec) : tree =
  Logger.render vs;
  match vs with
  | Vs_const k -> Leaf k
  | Vs_list vss -> List (List.map vss ~f:render)
  | Vs_comp comp_spec ->
      let path = perform Alloc_pt in
      let view =
        {
          comp_spec;
          dec = Idle;
          st_store = St_store.empty;
          eff_q = Job_q.empty;
          children = Leaf Unit;
        }
      in
      perform (Update_ent (path, view));
      perform (Checkpoint { msg = "Render"; checkpoint = Render_check path });
      let { comp; arg } = comp_spec in
      let ({ param; body } : comp_def) = perform (Lookup_comp comp) in
      let env = Env.extend Env.empty ~id:param ~value:arg in
      let vs =
        (eval_mult |> env_h ~env |> ptph_h ~ptph:(path, P_init)) body
        |> vs_of_value_exn
      in
      let t = render vs in
      let entry = perform (Lookup_ent path) in
      perform (Update_ent (path, { entry with children = t }));
      perform (Checkpoint { msg = "Rendered"; checkpoint = Render_finish path });
      Path path

let rec update (path : Path.t) (arg : value) : bool =
  Logger.update path arg;
  perform
    (Checkpoint { msg = "Render (update)"; checkpoint = Render_check path });
  let { comp_spec = { comp; _ }; children; _ } = perform (Lookup_ent path) in
  let ({ param; body } : comp_def) = perform (Lookup_comp comp) in
  perform (Set_dec (path, Idle));
  perform (Set_arg (path, arg));
  let env = Env.extend Env.empty ~id:param ~value:arg in
  let vs =
    (eval_mult |> env_h ~env |> ptph_h ~ptph:(path, P_succ)) body
    |> vs_of_value_exn
  in

  let old_tree = children in
  (* TODO: We assume that updates from a younger sibling to an older
               sibling are not dropped, while those from an older sibling to a
               younger sibling are. That's why we are resetting the children
               list and then snoc each child again in the reconcile function. We
               should verify this behavior. *)
  (* NOTE: We don't do this any more, since we are modelling as an
               'in-place' update now *)
  (*let ent = perform (Lookup_ent path) in*)
  (*perform (Update_ent (path, { ent with children = [] }));*)
  let new_tree, updated = reconcile old_tree vs in
  let dec = perform (Get_dec path) in
  let ent = perform (Lookup_ent path) in
  let updated =
    perform (Update_ent (path, { ent with children = new_tree }));
    updated || Decision.(dec <> Idle)
  in
  if updated then
    perform
      (Checkpoint { msg = "Rendered (update)"; checkpoint = Render_finish path })
  else
    perform
      (Checkpoint
         { msg = "Render canceled (update)"; checkpoint = Render_cancel path });
  updated

and reconcile (old_tree : tree) (vs : view_spec) : tree * bool =
  Logger.reconcile old_tree vs;
  match (old_tree, vs) with
  | Leaf k, Vs_const k' when equal_const k k' -> (Leaf k, false)
  | Path path, (Vs_comp { comp; arg } as vs) ->
      let { comp_spec = { comp = comp'; _ }; _ } = perform (Lookup_ent path) in
      if Id.(comp = comp') then (Path path, update path arg)
      else (render vs, true)
  | List ts, Vs_list vss ->
      let len_ts = List.length ts in
      let len_vs = List.length vss in
      let ts, vs =
        if len_ts < len_vs then
          (ts @ List.init (len_vs - len_ts) ~f:(fun _ -> Leaf Unit), vss)
        else (List.take ts len_vs, vss)
      in
      let new_ts, updated = List.map2_exn ts vs ~f:reconcile |> List.unzip in
      (List new_ts, List.exists updated ~f:Fn.id)
  | _, vs -> (render vs, true)

let rec visit (t : tree) : bool =
  Logger.visit t;
  match t with
  | Leaf _ -> false
  | List ts -> List.fold ts ~init:false ~f:(fun acc t -> visit t || acc)
  | Path path ->
      let dec = perform (Get_dec path) in
      let updated =
        match dec with
        | Retry -> raise Unreachable
        | Idle ->
            let { children; _ } = perform (Lookup_ent path) in
            visit children
        | Update ->
            let { comp_spec = { arg; _ }; _ } = perform (Lookup_ent path) in
            update path arg
      in
      updated

let rec commit_effs (t : tree) : unit =
  Logger.commit_effs t;
  match t with
  | Leaf _ -> ()
  | List ts -> List.iter ts ~f:commit_effs
  | Path path ->
      let { children; _ } = perform (Lookup_ent path) in
      commit_effs children;

      (* Refetch the entry, as committing effects of children may change it *)
      let { eff_q; _ } = perform (Lookup_ent path) in
      (Job_q.iter eff_q ~f:(fun { body; env; _ } ->
           (eval |> env_h ~env |> ptph_h ~ptph:(path, P_effect)) body |> ignore);
       (* Refetch the entry, as committing effects may change it *)
       let ent = perform (Lookup_ent path) in
       perform (Update_ent (path, { ent with eff_q = Job_q.empty })));
      perform
        (Checkpoint { msg = "After effects"; checkpoint = Effects_finish path })

let rec top_exp : Prog.t -> Expr.hook_free_t = function
  | Expr e -> e
  | Comp (_, p) -> top_exp p

let rec collect : Prog.t -> Def_tab.t = function
  | Expr _ -> Def_tab.empty
  | Comp ({ name = comp; param; body }, p) ->
      collect p |> Def_tab.extend ~comp ~comp_def:{ param; body }

let step_prog (deftab : Def_tab.t) (top_exp : Expr.hook_free_t) : tree =
  Logger.step_prog deftab top_exp;
  let vs = top_exp |> eval |> vs_of_value_exn in
  render vs

let step_path (t : tree) : bool =
  Logger.step_path t;
  commit_effs t;
  visit t

type 'recording run_info = {
  steps : int;
  mem : Memory.t;
  treemem : Tree_mem.t;
  output : string;
  recording : 'recording;
}

let run (type recording) ?(fuel : int option)
    ~(recorder : (module Recorder_intf.Intf with type recording = recording))
    (prog : Prog.t) : recording run_info =
  Logger.run prog;

  let deftab = collect prog in
  let top_exp = top_exp prog in

  let driver () =
    let cnt = ref 1 in
    Logs.info (fun m -> m "Step prog %d" !cnt);

    let root = step_prog deftab top_exp in
    let rec step () =
      Logs.info (fun m -> m "Step path %d" (!cnt + 1));
      if step_path root then (
        Int.incr cnt;
        match fuel with Some n when !cnt >= n -> () | _ -> step ())
    in
    step ();
    !cnt
  in

  let driver () =
    let open (val recorder) in
    event_h ~recording:emp_recording driver ()
  in
  let driver () = treemem_h ~treemem:Tree_mem.empty driver () in
  let driver () = deftab_h ~deftab driver () in
  let driver () = mem_h ~mem:Memory.empty driver () in
  let driver () = io_h ~output:"" driver () in
  let (((steps, recording), treemem), mem), output = driver () in
  { steps; mem; treemem; output; recording }

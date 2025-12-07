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

let ph_h (type a b) (f : a -> b) (x : a) : ph:phase -> b =
  match f x with
  | v ->
      fun ~ph ->
        Logger.ph ph `Ret;
        v
  | effect Rd_pt, k -> (
      fun ~ph ->
        Logger.ph ph `Rd_pt;
        match ph with
        | P_init pt | P_succ pt -> continue k pt ~ph
        | P_normal -> raise Invalid_phase)
  | effect Rd_ph, k ->
      fun ~ph ->
        Logger.ph ph `Rd_ph;
        continue k ph ~ph

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

let view_h (type a b) (f : a -> b) (x : a) : view:view -> b * view =
  match f x with
  | v ->
      fun ~view ->
        Logger.view view `Ret;
        (v, view)
  | effect View_lookup_st label, k ->
      fun ~view ->
        Logger.view view (`View_lookup_st label);
        continue k (St_store.lookup view.st_store ~label) ~view
  | effect View_update_st (label, vq), k ->
      fun ~view ->
        Logger.view view (`View_update_st (label, vq));
        continue k ()
          ~view:
            {
              view with
              st_store = St_store.update view.st_store ~label ~value:vq;
            }
  | effect View_get_dec, k ->
      fun ~view ->
        Logger.view view `View_get_dec;
        continue k view.dec ~view
  | effect View_set_dec dec, k ->
      fun ~view ->
        Logger.view view (`View_set_dec dec);
        continue k () ~view:{ view with dec }
  | effect View_add_dec dec, k ->
      fun ~view ->
        Logger.view view (`View_add_dec dec);
        continue k () ~view:{ view with dec = Decision.(view.dec + dec) }
  | effect View_enq_eff clos, k ->
      fun ~view ->
        Logger.view view (`View_enq_eff clos);
        continue k () ~view:{ view with eff_q = Job_q.enqueue view.eff_q clos }
  | effect View_flush_eff, k ->
      fun ~view ->
        Logger.view view `View_flush_eff;
        continue k () ~view:{ view with eff_q = Job_q.empty }
  | effect View_get_comp_name, k ->
      fun ~view ->
        Logger.view view `View_get_comp_name;
        continue k view.comp_spec.comp ~view

let treemem_h (type a b) (f : a -> b) (x : a) :
    treemem:Tree_mem.t -> b * Tree_mem.t =
  match f x with
  | v ->
      fun ~treemem ->
        Logger.treemem treemem `Ret;
        (v, treemem)
  | effect Tree_lookup_st (path, label), k ->
      fun ~treemem ->
        Logger.treemem treemem (`Tree_lookup_st (path, label));
        continue k (Tree_mem.lookup_st treemem ~path ~label) ~treemem
  | effect Tree_update_st (path, label, (v, q)), k ->
      fun ~treemem ->
        Logger.treemem treemem (`Tree_update_st (path, label, (v, q)));
        continue k () ~treemem:(Tree_mem.update_st treemem ~path ~label (v, q))
  | effect Tree_get_dec path, k ->
      fun ~treemem ->
        Logger.treemem treemem (`Tree_get_dec path);
        continue k (Tree_mem.get_dec treemem ~path) ~treemem
  | effect Tree_add_dec (path, dec), k ->
      fun ~treemem ->
        Logger.treemem treemem (`Tree_add_dec (path, dec));
        continue k () ~treemem:(Tree_mem.add_dec treemem ~path dec)
  | effect Tree_set_dec (path, dec), k ->
      fun ~treemem ->
        Logger.treemem treemem (`Tree_set_dec (path, dec));
        continue k () ~treemem:(Tree_mem.set_dec treemem ~path dec)
  (* NOTE: in init *)
  | effect Alloc_pt, k ->
      fun ~treemem ->
        Logger.treemem treemem `Alloc_pt;
        continue k (Tree_mem.alloc_pt treemem) ~treemem
  | effect Lookup_view path, k ->
      fun ~treemem ->
        Logger.treemem treemem (`Lookup_view path);
        continue k (Tree_mem.lookup_view treemem ~path) ~treemem
  | effect Update_view (path, view), k ->
      fun ~treemem ->
        Logger.treemem treemem (`Update_view (path, view));
        continue k () ~treemem:(Tree_mem.update_view treemem ~path view)
  | effect Tree_flush_eff path, k ->
      fun ~treemem ->
        Logger.treemem treemem (`Tree_flush_eff path);
        continue k () ~treemem:(Tree_mem.flush_eff treemem ~path)
  | effect Tree_get_comp_name path, k ->
      fun ~treemem ->
        Logger.treemem treemem (`Tree_get_comp_name path);
        let view = Tree_mem.lookup_view treemem ~path in
        let comp_name = view.comp_spec.comp in
        continue k comp_name ~treemem

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

let bool_of_value_exn v = v |> Value.to_bool |> value_exn Type_error
let string_of_value_exn v = v |> Value.to_string |> value_exn Type_error
let addr_of_value_exn v = v |> Value.to_addr |> value_exn Type_error
let vs_of_value_exn v = v |> Value.to_vs |> value_exn Type_error
let clos_of_value_exn v = v |> Value.to_clos |> value_exn Type_error

let build_app_env ({ self; param; env; _ } as cl : clos) (arg : value) : Env.t =
  (* The function name is bound before the parameters, according to js semantics
     (ECMA-262 14th edition, p.347, "15.2.5 Runtime Semantics:
     InstantiateOrdinaryFunctionExpression": "5. Perform !
     funcEnv.CreateImmutableBinding(name, false)."). *)
  let open Env in
  let env =
    match self with
    | None -> env
    | Some self -> extend env ~id:self ~value:(Clos cl)
  in
  extend env ~id:param ~value:arg

let rec eval : type a. a Expr.t -> value =
 fun expr ->
  Logger.eval expr;
  match expr.desc with
  | Const Unit ->
      Logs.info (fun m -> m "Unit");
      Const Unit
  | Const (Bool b) ->
      Logs.info (fun m -> m (if b then "True" else "False"));
      Const (Bool b)
  | Const (Int i) ->
      Logs.info (fun m -> m "Int");
      Const (Int i)
  | Const (String s) ->
      Logs.info (fun m -> m "String");
      Const (String s)
  | Var id ->
      Logs.info (fun m -> m "Var");
      let env = perform Rd_env in
      Env.lookup env ~id
  | Comp c ->
      Logs.info (fun m -> m "Comp");
      Comp c
  | View es ->
      Logs.info (fun m -> m "List");
      List_spec (List.map es ~f:(fun e -> eval e |> vs_of_value_exn))
  | Cond { pred; con; alt } ->
      Logs.info (fun m -> m "Cond");
      let p = eval pred |> bool_of_value_exn in
      if p then eval con else eval alt
  | Fn { self; param; body } ->
      Logs.info (fun m -> m "Func");
      Clos { self; param; body; env = perform Rd_env }
  | App { fn; arg } -> (
      match eval fn with
      | Clos cl ->
          Logs.info (fun m -> m "AppFunc");
          let env = build_app_env cl (eval arg) in
          perform (In_env env) eval cl.body
      | Comp comp ->
          Logs.info (fun m -> m "AppCom");
          Comp_spec { comp; arg = eval arg }
      | Set_clos { label; path } ->
          (* Argument to the setter should be a setting thunk *)
          let clos = eval arg |> clos_of_value_exn in
          (match perform Rd_ph with
          | (P_init self_pt | P_succ self_pt) when Path.(path = self_pt) ->
              Logs.info (fun m -> m "AppSetComp");
              perform
                (Checkpoint
                   {
                     msg = "State update queued (during render)";
                     component_info = None;
                     checkpoint = Hook_eval Setter_call;
                     loc = Some expr.loc;
                   });
              perform (View_add_dec Decision.chk);
              let v, q = perform (View_lookup_st label) in
              perform (View_update_st (label, (v, Job_q.enqueue q clos)))
          | P_normal ->
              Logs.info (fun m -> m "AppSetNormal");
              perform
                (Checkpoint
                   {
                     msg = "State update queued (in effect)";
                     component_info = None;
                     checkpoint = Hook_eval Setter_call;
                     loc = Some expr.loc;
                   });
              perform (Tree_add_dec (path, Decision.chk));
              let v, q = perform (Tree_lookup_st (path, label)) in
              perform (Tree_update_st (path, label, (v, Job_q.enqueue q clos)))
          | _ -> raise Invalid_phase);
          Const Unit
      | _ -> raise Type_error)
  | Let { id; bound; body } ->
      Logs.info (fun m -> m "LetBind");
      let value = eval bound in
      let env = Env.extend (perform Rd_env) ~id ~value in
      perform (In_env env) eval body
  | Stt { label; stt; set; init; body } -> (
      let path = perform Rd_pt in
      match perform Rd_ph with
      | P_init _ ->
          Logs.info (fun m -> m "SttBind");
          perform
            (Checkpoint
               {
                 msg = "Initializing state";
                 component_info = None;
                 checkpoint = Hook_eval Use_state;
                 loc = Some expr.loc;
               });
          let v = eval init in
          perform (View_update_st (label, (v, Job_q.empty)));
          let env =
            perform Rd_env
            |> Env.extend ~id:stt ~value:v
            |> Env.extend ~id:set ~value:(Set_clos { label; path })
          in
          perform (In_env env) eval body
      | P_succ _ ->
          Logs.info (fun m -> m "SttReBind");
          perform
            (Checkpoint
               {
                 msg = "Processing state updates";
                 component_info = None;
                 checkpoint = Hook_eval Use_state;
                 loc = Some expr.loc;
               });
          let v_old, q = perform (View_lookup_st label) in
          perform (View_update_st (label, (v_old, Job_q.empty)));
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
          if Value.(v_old <> v) then perform (View_add_dec Decision.eff);
          let _, q = perform (View_lookup_st label) in
          perform (View_update_st (label, (v, q)));
          perform (In_env env) eval body
      | P_normal -> raise Invalid_phase)
  | Eff e ->
      Logs.info (fun m -> m "Eff");
      (match perform Rd_ph with P_normal -> raise Invalid_phase | _ -> ());
      perform
        (Checkpoint
           {
             msg = "Effect scheduled";
             component_info = None;
             checkpoint = Hook_eval Use_effect;
             loc = Some expr.loc;
           });
      let env = perform Rd_env in
      perform (View_enq_eff { self = None; param = Id.unit; body = e; env });
      Const Unit
  | Seq (e1, e2) ->
      Logs.info (fun m -> m "Seq");
      eval e1 |> ignore;
      eval e2
  | Uop { op; arg } ->
      Logs.info (fun m -> m "Uop");
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
      Logs.info (fun m -> m "Bop");
      let v1 = eval left in
      let v2 = eval right in
      let k =
        match (op, v1, v2) with
        | Eq, Const Unit, Const Unit -> Bool true
        | Eq, Const (Bool b1), Const (Bool b2) -> Bool Bool.(b1 = b2)
        | Eq, Const (Int i1), Const (Int i2) -> Bool (i1 = i2)
        | Eq, Const (String s1), Const (String s2) -> Bool String.(s1 = s2)
        | Eq, _, _ -> Bool false
        | Lt, Const (Int i1), Const (Int i2) -> Bool (i1 < i2)
        | Lt, Const (String s1), Const (String s2) -> Bool String.(s1 < s2)
        | Gt, Const (Int i1), Const (Int i2) -> Bool (i1 > i2)
        | Gt, Const (String s1), Const (String s2) -> Bool String.(s1 > s2)
        | Le, Const (Int i1), Const (Int i2) -> Bool (i1 <= i2)
        | Le, Const (String s1), Const (String s2) -> Bool String.(s1 <= s2)
        | Ge, Const (Int i1), Const (Int i2) -> Bool (i1 >= i2)
        | Ge, Const (String s1), Const (String s2) -> Bool String.(s1 >= s2)
        | Ne, Const Unit, Const Unit -> Bool false
        | Ne, Const (Bool b1), Const (Bool b2) -> Bool Bool.(b1 <> b2)
        | Ne, Const (Int i1), Const (Int i2) -> Bool (i1 <> i2)
        | Ne, Const (String s1), Const (String s2) -> Bool String.(s1 <> s2)
        | Ne, _, _ -> Bool true
        | And, Const (Bool b1), Const (Bool b2) -> Bool (b1 && b2)
        | Or, Const (Bool b1), Const (Bool b2) -> Bool (b1 || b2)
        | Plus, Const (Int i1), Const (Int i2) -> Int (i1 + i2)
        | Plus, Const (String s1), Const (String s2) -> String (s1 ^ s2)
        | Plus, Const (String s1), Const (Int i2) ->
            (* I know XD *)
            String (s1 ^ Int.to_string i2)
        | Minus, Const (Int i1), Const (Int i2) -> Int (i1 - i2)
        | Times, Const (Int i1), Const (Int i2) -> Int (i1 * i2)
        | Times, Const (String s1), Const (Int i2) ->
            String (String.concat (List.init i2 ~f:(fun _ -> s1)))
        | Div, Const (Int i1), Const (Int i2) -> Int (i1 / i2)
        | Mod, Const (Int i1), Const (Int i2) -> Int (Int.rem i1 i2)
        | _, _, _ -> raise Type_error
      in
      Const k
  | Alloc ->
      Logs.info (fun m -> m "Alloc");
      let addr = perform (Alloc_addr Obj.empty) in
      Addr addr
  | Get { obj; idx } ->
      Logs.info (fun m -> m "Get");
      let addr = eval obj |> addr_of_value_exn in
      let i = eval idx |> string_of_value_exn in
      let obj = perform (Lookup_addr addr) in
      Obj.lookup obj ~field:i
  | Set { obj; idx; value } ->
      Logs.info (fun m -> m "Set");
      let addr = eval obj |> addr_of_value_exn in
      let i = eval idx |> string_of_value_exn in
      let old_obj = perform (Lookup_addr addr) in
      let value = eval value in
      let new_obj = Obj.update old_obj ~field:i ~value in
      perform (Update_addr (addr, new_obj));
      Const Unit
  | Print s ->
      Logs.info (fun m -> m "Print");
      perform (Print (string_of_value_exn (eval s)));
      Const Unit

let rec eval_mult : type a. ?re_render:int -> a Expr.t -> value =
 fun ?(re_render = 1) expr ->
  Logger.eval_mult expr;

  (* This is a hack only used for testing non-termination. *)
  (try if re_render >= perform Re_render_limit then raise Too_many_re_renders
   with Stdlib.Effect.Unhandled Re_render_limit -> ());

  perform View_flush_eff;
  perform (View_set_dec { (perform View_get_dec) with chk = false });
  let path = perform Rd_pt in
  let comp_name = perform View_get_comp_name in
  let decision = perform View_get_dec in
  perform
    (Checkpoint
       {
         msg = Printf.sprintf "Render pass (attempt %d)" re_render;
         component_info = Some (comp_name, decision);
         checkpoint = Render_check path;
         loc = None;
       });
  let v = eval expr in
  if (perform View_get_dec).chk then (
    Logs.info (fun m -> m "EvalMult");
    let re_render = re_render + 1 in
    perform
      (Checkpoint
         {
           msg = "Re-rendering due to state change";
           component_info = Some (comp_name, decision);
           checkpoint = Retry_start (re_render, path);
           loc = None;
         });
    ph_h ~ph:(P_succ path) (eval_mult ~re_render) expr)
  else (
    Logs.info (fun m -> m "EvalOnce");
    v)

let rec init (vs : view_spec) : tree =
  Logger.init vs;
  match vs with
  | Vs_const k ->
      Logs.info (fun m -> m "InitConst");
      T_const k
  | Vs_clos cl ->
      Logs.info (fun m -> m "InitClos");
      T_clos cl
  | Vs_list vss ->
      Logs.info (fun m -> m "InitArray");
      T_list (List.map vss ~f:init)
  | Vs_comp ({ comp; arg } as comp_spec) ->
      Logs.info (fun m -> m "InitCom");
      let path = perform Alloc_pt in
      let view =
        {
          comp_spec;
          dec = Decision.idle;
          st_store = St_store.empty;
          eff_q = Job_q.empty;
          children = T_const Unit;
        }
      in
      let ({ param; body } : comp_def) = perform (Lookup_comp comp) in
      let env = Env.extend Env.empty ~id:param ~value:arg in
      perform
        (Checkpoint
           {
             msg = "Mounting";
             component_info = Some (comp, Decision.idle);
             checkpoint = Render_check path;
             loc = None;
           });
      let vs, view =
        (eval_mult |> env_h ~env |> view_h ~view |> ph_h ~ph:(P_init path)) body
      in
      let vs = vs_of_value_exn vs in
      perform (Update_view (path, view));
      let t = init vs in
      perform
        (Update_view (path, { view with dec = Decision.eff; children = t }));
      perform
        (Checkpoint
           {
             msg = "Mounted";
             component_info = Some (comp, Decision.eff);
             checkpoint = Render_finish path;
             loc = None;
           });
      T_path path

let rec reconcile (old_tree : tree) (vs : view_spec) : tree =
  Logger.reconcile old_tree vs;
  match (old_tree, vs) with
  | T_list ts, Vs_list vss ->
      Logs.info (fun m -> m "ReconcileArray");
      let len_ts = List.length ts in
      let len_vs = List.length vss in
      let ts, vs =
        if len_ts < len_vs then
          (ts @ List.init (len_vs - len_ts) ~f:(fun _ -> T_const Unit), vss)
        else (List.take ts len_vs, vss)
      in
      let new_ts = List.map2_exn ts vs ~f:reconcile in
      T_list new_ts
  | T_path path, (Vs_comp { comp; arg } as vs) ->
      let ({ comp_spec = { comp = comp'; _ }; children = old_tree; _ } as view)
          =
        perform (Lookup_view path)
      in
      if Id.(comp = comp') then (
        Logs.info (fun m -> m "ReconcileComEffect");
        Logger.update path arg;
        perform
          (Checkpoint
             {
               msg = "Re-rendering (props changed)";
               component_info = Some (comp, Decision.idle);
               checkpoint = Render_check path;
               loc = None;
             });
        let ({ param; body } : comp_def) = perform (Lookup_comp comp) in
        let env = Env.extend Env.empty ~id:param ~value:arg in
        let vs, view =
          (eval_mult |> env_h ~env
          |> view_h ~view:{ view with comp_spec = { comp; arg } }
          |> ph_h ~ph:(P_succ path))
            body
        in
        let vs = vs_of_value_exn vs in

        let new_tree = reconcile old_tree vs in
        perform
          (Update_view
             (path, { view with dec = Decision.eff; children = new_tree }));

        perform
          (Checkpoint
             {
               msg = "Re-render complete";
               component_info = Some (comp, Decision.eff);
               checkpoint = Render_finish path;
               loc = None;
             });
        T_path path)
      else (
        Logs.info (fun m -> m "ReconcileComNew");
        init vs)
  | _, vs ->
      Logs.info (fun m -> m "ReconcileOther");
      init vs

let rec check (t : tree) : bool =
  Logger.check t;
  match t with
  | T_const _ ->
      Logs.info (fun m -> m "CheckConst");
      false
  | T_clos _ ->
      Logs.info (fun m -> m "CheckClos");
      false
  | T_list ts ->
      Logs.info (fun m -> m "CheckArray");
      let updated = List.map ts ~f:check in
      List.exists updated ~f:Fn.id
  | T_path path ->
      let ({ comp_spec = { comp; arg }; children; _ } as view) =
        perform (Lookup_view path)
      in
      if (perform (Tree_get_dec path)).chk then (
        perform
          (Checkpoint
             {
               msg = "Re-rendering (state changed)";
               component_info = Some (comp, perform (Tree_get_dec path));
               checkpoint = Render_check path;
               loc = None;
             });
        let ({ param; body } : comp_def) = perform (Lookup_comp comp) in
        let env = Env.extend Env.empty ~id:param ~value:arg in
        let vs, view =
          (eval_mult |> env_h ~env |> view_h ~view |> ph_h ~ph:(P_succ path))
            body
        in
        let vs = vs_of_value_exn vs in

        if view.dec.eff then (
          Logs.info (fun m -> m "CheckEffect");
          let children = reconcile children vs in
          perform (Update_view (path, { view with children }));
          perform
            (Checkpoint
               {
                 msg = "Rendered with updates";
                 component_info = Some (comp, view.dec);
                 checkpoint = Render_finish path;
                 loc = None;
               });
          true)
        else (
          Logs.info (fun m -> m "CheckNoEffect");
          let b = check children in
          perform (Update_view (path, view));
          perform
            (Checkpoint
               {
                 msg = "Render skipped (no changes)";
                 component_info = Some (comp, view.dec);
                 checkpoint = Render_cancel path;
                 loc = None;
               });
          b))
      else (
        Logs.info (fun m -> m "CheckIdle");
        check children)

let rec commit_effs (t : tree) : unit =
  Logger.commit_effs t;
  match t with
  | T_const _ ->
      Logs.info (fun m -> m "CommitEffsConst");
      ()
  | T_clos _ ->
      Logs.info (fun m -> m "CommitEffsClos");
      ()
  | T_list ts ->
      Logs.info (fun m -> m "CommitEffsArray");
      List.iter ts ~f:commit_effs
  | T_path path ->
      let { children; dec; eff_q; _ } = perform (Lookup_view path) in
      commit_effs children;

      (* Refetch the view, as committing effects of children may change it *)
      if dec.eff then (
        Logs.info (fun m -> m "CommitEffsPath");
        Job_q.iter eff_q ~f:(fun { body; env; _ } ->
            (eval |> env_h ~env |> ph_h ~ph:P_normal) body |> ignore);
        perform
          (Tree_set_dec
             (path, { (perform (Tree_get_dec path)) with eff = false })))
      else Logs.info (fun m -> m "CommitEffsPathIdle");

      let comp_name = perform (Tree_get_comp_name path) in
      let dec = perform (Tree_get_dec path) in
      perform
        (Checkpoint
           {
             msg = "Effects committed";
             component_info = Some (comp_name, dec);
             checkpoint = Effects_finish path;
             loc = None;
           })

let rec top_exp : Prog.t -> Expr.hook_free_t = function
  | Expr e -> e
  | Comp (_, p) -> top_exp p

let rec collect : Prog.t -> Def_tab.t = function
  | Expr _ -> Def_tab.empty
  | Comp ({ name = comp; param; body }, p) ->
      collect p |> Def_tab.extend ~comp ~comp_def:{ param; body }

let rec handlers (t : tree) : clos list =
  match t with
  | T_const _ -> []
  | T_clos cl -> [ cl ]
  | T_list ts -> List.concat_map ts ~f:handlers
  | T_path path ->
      let { children; _ } = perform (Lookup_view path) in
      handlers children

let step_loop i (t : tree) : unit =
  Logger.step_loop t;
  (* TODO: exn? *)
  let clos = List.nth_exn (handlers t) i in
  (eval |> env_h ~env:(build_app_env clos (Const Unit)) |> ph_h ~ph:P_normal)
    clos.body
  |> ignore;
  perform
    (Checkpoint
       {
         msg = "Event handler executed";
         component_info = None;
         checkpoint = Event i;
         loc = None;
       })

type 'recording run_info = {
  steps : int;
  mem : Memory.t;
  treemem : Tree_mem.t;
  output : string;
  recording : 'recording;
}

let run (type recording) ?(fuel : int option) ~event_q_handler
    ~(recorder : (module Recorder_intf.Intf with type recording = recording))
    (prog : Prog.t) : recording run_info =
  Logger.run prog;

  let deftab = collect prog in
  let top_exp = top_exp prog in

  let driver () =
    let cnt = ref 1 in
    Logs.info (fun m -> m "StepInit (%d)" !cnt);

    let vs = top_exp |> eval |> vs_of_value_exn in
    let root = init vs in
    perform (Set_root root);
    let rec step = function
      | M_paint ->
          Logs.info (fun m -> m "StepCheck (%d)" (!cnt + 1));
          if check root then (
            Int.incr cnt;
            match fuel with Some n when !cnt >= n -> () | _ -> step M_react)
          else step M_eloop
      | M_react ->
          Logs.info (fun m -> m "StepEffect (%d)" (!cnt + 1));
          commit_effs root;
          step M_paint
      | M_eloop -> (
          let i = perform Listen in
          match i with
          | None ->
              Logs.info (fun m -> m "Terminate");
              ()
          | Some i ->
              Logs.info (fun m -> m "StepEvent [event: %d]" i);
              let clos = List.nth_exn (handlers root) i in
              (eval
              |> env_h ~env:(build_app_env clos (Const Unit))
              |> ph_h ~ph:P_normal)
                clos.body
              |> ignore;
              perform
                (Checkpoint
                   {
                     msg = "Event handler executed";
                     component_info = None;
                     checkpoint = Event i;
                     loc = None;
                   });
              step M_paint)
    in
    step M_react;
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
  let driver () = event_q_handler driver () in
  let (((steps, recording), treemem), mem), output = driver () in
  { steps; mem; treemem; output; recording }

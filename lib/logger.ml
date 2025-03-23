open! Base
open Lib_domains
open Syntax
open Concrete_domains

let ph (ph : Phase.t) = function
  | `Ret ->
      Logs.debug (fun m -> m "ph_h Ret [ph: %a]" Sexp.pp_hum (sexp_of_phase ph))
  | `Rd_pt ->
      Logs.debug (fun m ->
          m "ph_h Rd_pt [ph: %a]" Sexp.pp_hum (sexp_of_phase ph))
  | `Rd_ph ->
      Logs.debug (fun m ->
          m "ph_h Rd_ph [ph: %a]" Sexp.pp_hum (sexp_of_phase ph))

let env env = function
  | `Ret ->
      Logs.debug (fun m ->
          m "env_h Ret [env: %a]" Sexp.pp_hum (Env.sexp_of_t env))
  | `Rd_env ->
      Logs.debug (fun m ->
          m "env_h Rd_env [env: %a]" Sexp.pp_hum (Env.sexp_of_t env))
  | `In_env env' ->
      Logs.debug (fun m ->
          m "env_h In_env [env: %a, env': %a]" Sexp.pp_hum (Env.sexp_of_t env)
            Sexp.pp_hum (Env.sexp_of_t env'))

let mem mem = function
  | `Ret ->
      Logs.debug (fun m ->
          m "mem_h Ret [mem: %a]" Sexp.pp_hum (Memory.sexp_of_t mem))
  | `Lookup_addr addr ->
      Logs.debug (fun m ->
          m "mem_h Lookup_addr [mem: %a, addr: %a]" Sexp.pp_hum
            (Memory.sexp_of_t mem) Sexp.pp_hum (Addr.sexp_of_t addr))
  | `Update_addr (addr, obj) ->
      Logs.debug (fun m ->
          m "mem_h Update_addr [mem: %a, addr: %a, obj: %a]" Sexp.pp_hum
            (Memory.sexp_of_t mem) Sexp.pp_hum (Addr.sexp_of_t addr) Sexp.pp_hum
            (Obj.sexp_of_t obj))
  | `Alloc_addr ->
      Logs.debug (fun m ->
          m "mem_h Alloc_addr [mem: %a]" Sexp.pp_hum (Memory.sexp_of_t mem))

let view view = function
  | `Ret ->
      Logs.debug (fun m ->
          m "view_h Ret [view: %a]" Sexp.pp_hum (sexp_of_view view))
  | `View_lookup_st label ->
      Logs.debug (fun m ->
          m "view_h View_lookup_st [view: %a, label: %a]" Sexp.pp_hum
            (sexp_of_view view) Sexp.pp_hum (Label.sexp_of_t label))
  | `View_update_st (label, (v, q)) ->
      Logs.debug (fun m ->
          m "view_h View_update_st [view: %a, label: %a, v: %a, q: %a]"
            Sexp.pp_hum (sexp_of_view view) Sexp.pp_hum (Label.sexp_of_t label)
            Sexp.pp_hum (sexp_of_value v) Sexp.pp_hum (Job_q.sexp_of_t q))
  | `View_get_dec ->
      Logs.debug (fun m ->
          m "view_h View_get_dec [view: %a]" Sexp.pp_hum (sexp_of_view view))
  | `View_add_dec dec ->
      Logs.debug (fun m ->
          m "view_h View_add_dec [view: %a, dec: %a]" Sexp.pp_hum
            (sexp_of_view view) Sexp.pp_hum (sexp_of_decision dec))
  | `View_set_dec dec ->
      Logs.debug (fun m ->
          m "view_h View_set_dec [view: %a, dec: %a]" Sexp.pp_hum
            (sexp_of_view view) Sexp.pp_hum (sexp_of_decision dec))
  | `View_enq_eff clos ->
      Logs.debug (fun m ->
          m "view_h View_enq_eff [view: %a, clos: %a]" Sexp.pp_hum
            (sexp_of_view view) Sexp.pp_hum (sexp_of_clos clos))
  | `View_flush_eff ->
      Logs.debug (fun m ->
          m "view_h View_flush_eff [view: %a]" Sexp.pp_hum (sexp_of_view view))
  | `View_get_comp_name ->
      Logs.debug (fun m ->
          m "view_h View_get_comp_name [view: %a]" Sexp.pp_hum
            (sexp_of_view view))

let treemem treemem = function
  | `Ret ->
      Logs.debug (fun m ->
          m "treemem_h Ret [treemem: %a]" Sexp.pp_hum
            (Tree_mem.sexp_of_t treemem))
  | `Tree_lookup_st (path, label) ->
      Logs.debug (fun m ->
          m "treemem_h Tree_lookup_st [treemem: %a, path: %a, label: %a]"
            Sexp.pp_hum
            (Tree_mem.sexp_of_t treemem)
            Sexp.pp_hum (Path.sexp_of_t path) Sexp.pp_hum
            (Label.sexp_of_t label))
  | `Tree_update_st (path, label, (v, q)) ->
      Logs.debug (fun m ->
          m
            "treemem_h Tree_update_st [treemem: %a, path: %a, label: %a, v: \
             %a, q: %a]"
            Sexp.pp_hum
            (Tree_mem.sexp_of_t treemem)
            Sexp.pp_hum (Path.sexp_of_t path) Sexp.pp_hum
            (Label.sexp_of_t label) Sexp.pp_hum (sexp_of_value v) Sexp.pp_hum
            (Job_q.sexp_of_t q))
  | `Tree_get_dec path ->
      Logs.debug (fun m ->
          m "treemem_h Tree_get_dec [treemem: %a, path: %a]" Sexp.pp_hum
            (Tree_mem.sexp_of_t treemem)
            Sexp.pp_hum (Path.sexp_of_t path))
  | `Tree_add_dec (path, dec) ->
      Logs.debug (fun m ->
          m "treemem_h Tree_add_dec [treemem: %a, path: %a, dec: %a]"
            Sexp.pp_hum
            (Tree_mem.sexp_of_t treemem)
            Sexp.pp_hum (Path.sexp_of_t path) Sexp.pp_hum (sexp_of_decision dec))
  | `Tree_set_dec (path, dec) ->
      Logs.debug (fun m ->
          m "treemem_h Tree_set_dec [treemem: %a, path: %a, dec: %a]"
            Sexp.pp_hum
            (Tree_mem.sexp_of_t treemem)
            Sexp.pp_hum (Path.sexp_of_t path) Sexp.pp_hum (sexp_of_decision dec))
  (*| `Enq_eff (path, clos) ->*)
  (*    Logs.debug (fun m ->*)
  (*        m "treemem_h Enq_eff [treemem: %a, path: %a, clos: %a]" Sexp.pp_hum*)
  (*          (Tree_mem.sexp_of_t treemem)*)
  (*          Sexp.pp_hum (Path.sexp_of_t path) Sexp.pp_hum (sexp_of_clos clos))*)
  | `Alloc_pt ->
      Logs.debug (fun m ->
          m "treemem_h Alloc_pt [treemem: %a]" Sexp.pp_hum
            (Tree_mem.sexp_of_t treemem))
  | `Lookup_view path ->
      Logs.debug (fun m ->
          m "treemem_h Lookup_view [treemem: %a, path: %a]" Sexp.pp_hum
            (Tree_mem.sexp_of_t treemem)
            Sexp.pp_hum (Path.sexp_of_t path))
  | `Update_view (path, view) ->
      Logs.debug (fun m ->
          m "treemem_h Update_view [treemem: %a, path: %a, view: %a]"
            Sexp.pp_hum
            (Tree_mem.sexp_of_t treemem)
            Sexp.pp_hum (Path.sexp_of_t path) Sexp.pp_hum (sexp_of_view view))
  | `Tree_flush_eff path ->
      Logs.debug (fun m ->
          m "treemem_h Tree_flush_eff [treemem: %a, path: %a]" Sexp.pp_hum
            (Tree_mem.sexp_of_t treemem)
            Sexp.pp_hum (Path.sexp_of_t path))
  | `Tree_get_comp_name path ->
      Logs.debug (fun m ->
          m "treemem_h Tree_get_comp_name [treemem: %a, path: %a]" Sexp.pp_hum
            (Tree_mem.sexp_of_t treemem)
            Sexp.pp_hum (Path.sexp_of_t path))

let deftab deftab = function
  | `Ret ->
      Logs.debug (fun m ->
          m "deftab_h Ret [deftab: %a]" Sexp.pp_hum (Def_tab.sexp_of_t deftab))
  | `Lookup_comp comp ->
      Logs.debug (fun m ->
          m "deftab_h Lookup_comp [deftab: %a, comp: %a]" Sexp.pp_hum
            (Def_tab.sexp_of_t deftab) Sexp.pp_hum (Id.sexp_of_t comp))
  | `Get_comp_env ->
      Logs.debug (fun m ->
          m "deftab_h Get_comp_env [deftab: %a]" Sexp.pp_hum
            (Def_tab.sexp_of_t deftab))

let io io = function
  | `Ret -> Logs.debug (fun m -> m "io_h Ret [%s]" io)
  | `Print s -> Logs.debug (fun m -> m "io_h Print [%s]" s)

let event event = function
  | `Ret ->
      Logs.debug (fun m ->
          m "event_h Ret [%a]" Sexp.pp_hum ([%sexp_of: int list] event))
  | `Listen ->
      Logs.debug (fun m ->
          m "event_h Listen [%a]" Sexp.pp_hum ([%sexp_of: int list] event))

let eval expr =
  Logs.debug (fun m -> m "eval %a" Sexp.pp_hum (Expr.sexp_of_t expr))

let eval_mult expr =
  Logs.debug (fun m -> m "eval_mult %a" Sexp.pp_hum (Expr.sexp_of_t expr))

let mount_tree path t =
  Logs.debug (fun m ->
      m "mount_tree [path: %a, t: %a]" Sexp.pp_hum (Path.sexp_of_t path)
        Sexp.pp_hum (sexp_of_tree t))

let init vs =
  Logs.debug (fun m -> m "init [vs: %a]" Sexp.pp (sexp_of_view_spec vs))

let update path arg =
  Logs.debug (fun m ->
      m "update [path: %a, arg: %a]" Sexp.pp (Path.sexp_of_t path) Sexp.pp
        (sexp_of_value arg))

let visit t = Logs.debug (fun m -> m "visit [t: %a]" Sexp.pp (sexp_of_tree t))

let reconcile old_tree vs =
  Logs.debug (fun m ->
      m "reconcile [old_tree: %a, vs: %a]" Sexp.pp (sexp_of_tree old_tree)
        Sexp.pp (sexp_of_view_spec vs))

let commit_effs t =
  Logs.debug (fun m -> m "commit_effs1 [t: %a]" Sexp.pp (sexp_of_tree t))

let eval_top prog =
  Logs.debug (fun m -> m "eval_top %a" Sexp.pp_hum (Prog.sexp_of_t prog))

let step_prog deftab top_exp =
  Logs.debug (fun m ->
      m "step_prog %a %a" Sexp.pp_hum (Def_tab.sexp_of_t deftab) Sexp.pp_hum
        (Expr.sexp_of_t top_exp))

let step_path t =
  Logs.debug (fun m -> m "step_path %a" Sexp.pp_hum (sexp_of_tree t))

let step_loop t =
  Logs.debug (fun m -> m "step_loop %a" Sexp.pp_hum (sexp_of_tree t))

let run prog =
  Logs.debug (fun m -> m "run %a" Sexp.pp_hum (Prog.sexp_of_t prog))

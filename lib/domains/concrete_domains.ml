open! Base
open Syntax

module M : Domains.S = struct
  module rec T : (Domains.T with type path = int) = struct
    type path = Path.t [@@deriving sexp_of, equal]
    type env = Env.t [@@deriving sexp_of]
    type addr = Addr.t [@@deriving sexp_of]
    type obj = Obj.t [@@deriving sexp_of]
    type st_store = St_store.t [@@deriving sexp_of]
    type job_q = Job_q.t [@@deriving sexp_of]

    type comp_def = { param : Id.t; body : Expr.hook_full_t }
    [@@deriving sexp_of]

    type clos = {
      self : Id.t option;
      param : Id.t;
      body : Expr.hook_free_t;
      env : env;
    }
    [@@deriving sexp_of]

    type const = Unit | Bool of bool | Int of int | String of string
    [@@deriving sexp_of, equal]

    type value =
      | Const of const
      | Addr of addr
      | Comp of Id.t
      | Clos of clos
      | Set_clos of set_clos
      | List_spec of view_spec list
      | Comp_spec of comp_spec

    and set_clos = { label : Label.t; path : path }
    and comp_spec = { comp : Id.t; arg : value }

    and view_spec =
      | Vs_const of const
      | Vs_clos of clos
      | Vs_list of view_spec list
      | Vs_comp of comp_spec
    [@@deriving sexp_of]

    type phase = P_init of path | P_succ of path | P_normal
    [@@deriving sexp_of]

    type decision = Idle | Retry | Update [@@deriving sexp_of]
    type mode = M_react | M_eloop

    type tree =
      | T_const of const
      | T_clos of clos
      | T_list of tree list
      | T_path of path
    [@@deriving sexp_of]

    type view = {
      comp_spec : comp_spec;
      dec : decision;
      st_store : st_store;
      eff_q : job_q;
      children : tree;
    }
    [@@deriving sexp_of]
  end

  and Path : (Domains.Path with type t = T.path) = Int

  and Env : (Domains.Env with type value = T.value and type t = T.env) = struct
    type value = T.value [@@deriving sexp_of]
    type t = value Id.Map.t [@@deriving sexp_of]

    let empty = Id.Map.empty
    let lookup env ~id = Map.find_exn env id
    let extend env ~id ~value = Map.set env ~key:id ~data:value
    let of_alist (a : (Id.t * value) list) = Map.of_alist_exn (module Id) a
  end

  and Addr : (Domains.Addr with type t = T.addr) = Int

  and Obj : (Domains.Obj with type value = T.value and type t = T.obj) = struct
    type value = T.value [@@deriving sexp_of]
    type t = value Id.Map.t [@@deriving sexp_of]

    let empty = Id.Map.empty

    let lookup obj ~field =
      Map.find obj field |> Option.value ~default:T.(Const Unit)

    let update obj ~field ~value = Map.set obj ~key:field ~data:value
  end

  and Memory : (Domains.Memory with type obj = T.obj and type addr = T.addr) =
  struct
    type obj = T.obj [@@deriving sexp_of]
    type addr = T.addr [@@deriving sexp_of]
    type t = obj Map.M(Addr).t [@@deriving sexp_of]

    let empty = Map.empty (module Addr)
    let alloc = Map.length
    let lookup memory ~addr = Map.find_exn memory addr
    let update memory ~addr ~obj = Map.set memory ~key:addr ~data:obj
  end

  and St_store :
    (Domains.St_store
      with type value = T.value
       and type job_q = T.job_q
       and type t = T.st_store) = struct
    type value = T.value [@@deriving sexp_of]
    type job_q = Job_q.t [@@deriving sexp_of]
    type t = (value * job_q) Label.Map.t [@@deriving sexp_of]

    let empty = Label.Map.empty
    let lookup store ~label = Map.find_exn store label
    let update store ~label ~value = Map.set store ~key:label ~data:value
    let to_alist store = Map.to_alist ~key_order:`Increasing store
  end

  and Job_q : (Domains.Job_q with type elt := T.clos and type t = T.job_q) =
  Batched_queue.M (struct
    type t = T.clos [@@deriving sexp_of]
  end)

  module Tree_mem :
    Domains.Tree_mem
      with type value = T.value
       and type path = T.path
       and type job_q = T.job_q
       and type decision = T.decision
       and type clos = T.clos
       and type view = T.view = struct
    type value = T.value [@@deriving sexp_of]
    type path = Path.t [@@deriving sexp_of]
    type job_q = Job_q.t [@@deriving sexp_of]
    type decision = T.decision [@@deriving sexp_of]
    type clos = T.clos [@@deriving sexp_of]
    type view = T.view [@@deriving sexp_of]
    type t = view Map.M(Path).t [@@deriving sexp_of]

    let empty = Map.empty (module Path)

    open T

    let lookup_st (tree_mem : t) ~(path : path) ~(label : Label.t) :
        value * job_q =
      let { st_store; _ } = Map.find_exn tree_mem path in
      St_store.lookup st_store ~label

    let update_st tree_mem ~path ~label (v, q) =
      let ({ st_store; _ } as view) = Map.find_exn tree_mem path in
      let st_store = St_store.update st_store ~label ~value:(v, q) in
      Map.set tree_mem ~key:path ~data:{ view with st_store }

    let get_dec tree_mem ~path =
      let { dec; _ } = Map.find_exn tree_mem path in
      dec

    let set_dec tree_mem ~path dec =
      let view = Map.find_exn tree_mem path in
      Map.set tree_mem ~key:path ~data:{ view with dec }

    let set_arg tree_mem ~path arg =
      let ({ comp_spec; _ } as view) = Map.find_exn tree_mem path in
      Map.set tree_mem ~key:path
        ~data:{ view with comp_spec = { comp_spec with arg } }

    let flush_eff tree_mem ~path =
      let view = Map.find_exn tree_mem path in
      let eff_q = Job_q.empty in
      Map.set tree_mem ~key:path ~data:{ view with eff_q }

    let root_pt (tree_mem : t) =
      ignore tree_mem;
      0

    let alloc_pt = Map.length
    let lookup_view tree_mem ~path = Map.find_exn tree_mem path

    let update_view tree_mem ~path view =
      Logs.debug (fun m ->
          m "update_view: %a" Sexp.pp_hum (Path.sexp_of_t path));
      Map.set tree_mem ~key:path ~data:view
  end

  module Def_tab :
    Domains.Def_tab with type comp_def = T.comp_def and type env = T.env =
  struct
    type comp_def = T.comp_def [@@deriving sexp_of]
    type t = comp_def Id.Map.t [@@deriving sexp_of]
    type env = T.env

    let empty = Id.Map.empty
    let lookup def_tab ~comp = Map.find_exn def_tab comp

    let extend def_tab ~comp ~comp_def =
      Map.set def_tab ~key:comp ~data:comp_def
  end

  include T

  module Value = struct
    type nonrec view_spec = view_spec
    type nonrec clos = clos
    type nonrec addr = addr
    type t = value

    let to_bool = function Const (Bool b) -> Some b | _ -> None
    let to_int = function Const (Int i) -> Some i | _ -> None

    let to_string = function
      | Const (String s) -> Some s
      | Const (Int i) -> Some (Int.to_string i)
      | Const (Bool b) -> Some (Bool.to_string b)
      | _ -> None

    let to_addr = function Addr l -> Some l | _ -> None

    let to_vs = function
      | Const k -> Some (Vs_const k)
      | Clos cl -> Some (Vs_clos cl)
      | List_spec l -> Some (Vs_list l)
      | Comp_spec c -> Some (Vs_comp c)
      | _ -> None

    let to_clos = function Clos c -> Some c | _ -> None

    let equal v1 v2 =
      match (v1, v2) with
      | Const Unit, Const Unit -> true
      | Const (Bool b1), Const (Bool b2) -> Bool.(b1 = b2)
      | Const (Int i1), Const (Int i2) -> i1 = i2
      | Addr l1, Addr l2 -> Addr.(l1 = l2)
      | _, _ -> false

    let ( = ) = equal
    let ( <> ) v1 v2 = not (v1 = v2)
  end

  module Phase = struct
    type t = phase = P_init of path | P_succ of path | P_normal
    [@@deriving equal]

    let ( = ) = equal
    let ( <> ) p1 p2 = not (p1 = p2)
  end

  module Decision = struct
    type t = decision = Idle | Retry | Update [@@deriving equal]

    let ( = ) = equal
    let ( <> ) d1 d2 = not (d1 = d2)
  end

  module Mode = struct
    type t = mode = M_react | M_eloop [@@deriving equal]

    let ( = ) = equal
    let ( <> ) m1 m2 = not (m1 = m2)
  end
end

include M

type state_entry = { label : string; value : string; queue_size : int }
[@@deriving yojson_of]

type decision_info = { chk : bool; eff : bool } [@@deriving yojson_of]

type tree = {
  path : string;
  name : string;
  children : tree list;
  st_store : state_entry list option;
  eff_q_size : int option;
  dec : decision_info option;
  arg : string option;
}
[@@deriving yojson_of]

type view = { msg : string; tree : tree } [@@deriving yojson_of]

type recording = {
  checkpoints : view list;
  root : Lib_domains.Concrete_domains.tree option;
  log : string;
}
[@@deriving yojson_of]

include Recorder_intf.Intf with type recording := recording

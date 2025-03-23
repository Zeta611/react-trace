type tree = { path : string; name : string; children : tree list }
and view = { msg : string; tree : tree } [@@deriving yojson_of]

and recording = {
  checkpoints : view list;
  root : Lib_domains.Concrete_domains.tree option;
  log : string;
}
[@@deriving yojson_of]

include Recorder_intf.Intf with type recording := recording

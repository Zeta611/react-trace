include
  Recorder_intf.Intf
    with type recording =
      (string * PrintBox.t) list * Lib_domains.Concrete_domains.tree option

open! Base
open Stdio
open React_trace
open Lib_domains

let print_position (outx : Out_channel.t) (lexbuf : Lexing.lexbuf) : unit =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  Out_channel.fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error (lexbuf : Lexing.lexbuf) : Syntax.Prog.t =
  Parser.prog Lexer.read lexbuf

let get_program (filename : string) : Syntax.Prog.t =
  let filename, inx =
    if String.(filename = "-") then ("<stdin>", In_channel.stdin)
    else (filename, In_channel.create filename)
  in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };

  match parse_with_error lexbuf with
  | prog ->
      In_channel.close inx;
      prog
  | exception Parser.Error ->
      Out_channel.fprintf stderr "%a: syntax error\n" print_position lexbuf;
      In_channel.close inx;
      Stdlib.exit 2

let with_re_render_limit_handler run prog ~re_render_limit =
  match re_render_limit with
  | None -> run prog
  | Some re_render_limit -> Interp.re_render_limit_h run prog ~re_render_limit

let () =
  let module Arg = Stdlib.Arg in
  let module Sys = Stdlib.Sys in
  let module Filename = Stdlib.Filename in
  let filename = ref "" in
  let opt_pp = ref false in
  let opt_parse_js = ref false in
  let opt_fuel = ref None in
  let opt_re_render_limit = ref None in
  let opt_report = ref false in
  let opt_verbosity = ref Logs.Info in
  let opt_event_q = ref [] in

  let usage_msg =
    "Usage : " ^ Filename.basename Sys.argv.(0) ^ " [-option] [filename] "
  in
  let speclist =
    [
      ("-pp", Arg.Unit (fun _ -> opt_pp := true), "Pretty-print program");
      ( "-parse-js",
        Arg.Unit (fun _ -> opt_parse_js := true),
        "Parse JS file with Flow" );
      ( "-verbose",
        Arg.Unit (fun _ -> opt_verbosity := Logs.Debug),
        "Verbose mode" );
      ( "-report",
        Arg.Unit (fun _ -> opt_report := true),
        "Report the view trees" );
      ("-fuel", Arg.Int (fun n -> opt_fuel := Some n), "Run with fuel");
      ( "-re-render-limit",
        Arg.Int (fun n -> opt_re_render_limit := Some n),
        "Run with re-render limit" );
      ( "-events",
        Arg.String
          (fun s ->
            opt_event_q := String.split ~on:',' s |> List.map ~f:Int.of_string),
        "Run with events (comma-separated list of event handlers to trigger, \
         e.g. '0,1,1,0')" );
    ]
  in
  Arg.parse speclist (fun x -> filename := x) usage_msg;
  if String.is_empty !filename then Arg.usage speclist usage_msg
  else if !opt_parse_js then (
    let js_syntax, _ = Js_syntax.parse !filename in
    print_endline (Js_syntax.show js_syntax);
    let prog = Js_syntax.convert js_syntax in
    Sexp.pp_hum Stdlib.Format.std_formatter (Syntax.Prog.sexp_of_t prog))
  else (
    Fmt_tty.setup_std_outputs ();
    Logs.set_reporter (Logs_fmt.reporter ());
    Logs.set_level (Some !opt_verbosity);

    let prog = get_program !filename in

    if !opt_pp then
      Sexp.pp_hum Stdlib.Format.std_formatter (Syntax.Prog.sexp_of_t prog)
    else
      let steps =
        if !opt_report then (
          let { Interp.steps; recording; output; _ } =
            with_re_render_limit_handler ~re_render_limit:!opt_re_render_limit
              (Interp.run ?fuel:!opt_fuel
                 ~event_q_handler:
                   (Default_event_q.event_h ~event_q:!opt_event_q)
                 ~recorder:(module Report_box_recorder))
              prog
          in
          fst recording |> List.rev
          |> List.iter ~f:(fun (msg, box) ->
                 Logs.info (fun m -> m "%s:\n" msg);
                 PrintBox_text.output Stdio.stdout box;
                 Out_channel.(
                   output_char stdout '\n';
                   flush stdout));
          Out_channel.print_endline output;
          steps)
        else
          let { Interp.steps; output; _ } =
            with_re_render_limit_handler ~re_render_limit:!opt_re_render_limit
              (Interp.run ?fuel:!opt_fuel
                 ~event_q_handler:
                   (Default_event_q.event_h ~event_q:!opt_event_q)
                 ~recorder:(module Default_recorder))
              prog
          in
          Out_channel.print_endline output;
          steps
      in
      Out_channel.(
        output_char stdout '\n';
        flush stdout);
      printf "Steps: %d\n" steps;
      Stdlib.exit (if Logs.err_count () > 0 then 1 else 0))

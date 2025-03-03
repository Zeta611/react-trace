{
open Lexing
open Parser

exception SyntaxError of string

let keywords =
  String_dict.of_alist_exn
    [
      ("true", TRUE);
      ("false", FALSE);
      ("not", NOT);
      ("mod", MOD);
      ("view", VIEW);
      ("fun", FUN);
      ("rec", REC);
      ("if", IF);
      ("then", THEN);
      ("else", ELSE);
      ("let", LET);
      ("useState", STT);
      ("in", IN);
      ("useEffect", EFF);
      ("print", PRINT);
    ]

let unescape_string s =
  let rec loop s =
    match s |> Seq.uncons with
    | Some ('\\', rest) ->
      (match Seq.uncons rest with
       | Some ('\\', rest) -> "\\" ^ loop rest
       | Some ('"', rest) -> "\"" ^ loop rest
       | _ -> raise (SyntaxError "Invalid escape sequence"))
    | Some (c, rest) -> String.make 1 c ^ loop rest
    | None -> ""
  in
  loop (String.to_seq s)
}

let escape_seq = "\\\\" | "\\\""
let ordinary_char = [^ '\\' '"' '\r' '\n']
let blank = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
let comp = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

let digit = ['0'-'9']
let int = digit+
let pow = ['e' 'E'] ['+' '-']? int
let real = ((int '.'? | (digit* '.' int))) pow?
let str = '"' (escape_seq | ordinary_char)* '"'

rule read =
  parse
  | blank     { read lexbuf }
  | newline   { new_line lexbuf; read lexbuf }
  | "()"      { UNIT }
  | int as n  { INT (int_of_string n) }
  | id as s   { match String_dict.find keywords s with Some s -> s | None -> ID s }
  | comp as c   { COMP c }
  | str as s  { STRING (String.sub s 1 (String.length s - 2) |> unescape_string) }
  | "{}"      { RECORD }
  | ":="      { ASSIGN }
  | '#'       { comment lexbuf }
  | "->"      { RARROW }
  | '='       { EQ }
  | '<'       { LT }
  | '>'       { GT }
  | "<>"      { NE }
  | "<="      { LE }
  | ">="      { GE }
  | "&&"      { AND }
  | "||"      { OR }
  | '+'       { PLUS }
  | '-'       { MINUS }
  | '*'       { TIMES }
  | '/'       { DIV }
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | '['       { LBRACK }
  | ']'       { RBRACK }
  | ','       { COMMA }
  | ';'       { SEMI }
  | ";;"      { SEMISEMI }
  | eof       { EOF }
  | _         { raise (SyntaxError ("Unexpected char: " ^ lexeme lexbuf)) }

and comment =
  parse
  | newline { read lexbuf }
  | eof     { EOF }
  | _       { comment lexbuf }

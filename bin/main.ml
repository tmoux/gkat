open Gkat
open Automaton
open IntAutomaton
open TestImpl
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Grammar.expr Lexer.token lexbuf with
  | Lexer.SyntaxError msg ->
    Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    exit (-1)
  | Grammar.Error ->
    Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let run_file filename equiv =
  let inx = In_channel.open_bin filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let e1 = parse_with_error lexbuf in
  let e2 = parse_with_error lexbuf in
  let eq = equiv e1 e2 in
  let sym = if eq then "=" else "=/=" in
  Printf.fprintf stdout "\n%s %s %s\n" (TestImpl.my_show_expr e1) sym (TestImpl.my_show_expr e2);
  Printf.fprintf stdout "%s\n" (if eq then "EQUAL" else "NOT EQUAL")

module MyAutomaton = AutomatonExn (IntAutomaton (MyTest) (MyTestOrd))

let _ = 
  let filename = ((Sys.argv)).(1) in
  run_file filename MyAutomaton.equiv
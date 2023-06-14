{
open Lexing
open Grammar

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let make_table num elems =
  let table = Hashtbl.create num in
  List.iter (fun (k, v) -> Hashtbl.add table k v) elems;
  table

let keywords =
  make_table 0 [
    ("b", TESTB);
    ("c", TESTC);
    ("d", TESTD);
    ("if", IF);
    ("then", THEN);
    ("else", ELSE);
    ("while", WHILE);
    ("do", DO);
    ("assert", ASSERT);
  ]
}

let number = ['0'-'9']['0'-'9']*
let whitespace = [' ' '\t']+
let line_ending = '\r' | '\n' | "\r\n"
let atom_first = ['a'-'z' 'A'-'Z' '_']
let atom_next = ['a'-'z' 'A'-'Z' '_' '-' '*' '/' '0'-'9']
let atom = atom_first atom_next*

rule token = parse
  | ";"
    { SEMICOLON }
  | "&&"
    { AND }
  | "||"
    { OR }
  | "!"
    { NOT }
  | '('
    { L_PARENS }
  | ')'
    { R_PARENS }
  | line_ending
    { new_line lexbuf; token lexbuf }
  | whitespace
    { token lexbuf }
  | number
    { let x = (int_of_string (Lexing.lexeme lexbuf)) in
      match x with
      | 0 -> B0
      | 1 -> B1
      | _ -> raise (SyntaxError ("Invalid number:" ^ lexeme lexbuf))
    }
  | atom
    {
      let input = lexeme lexbuf in
      begin try
        let kwd = Hashtbl.find keywords input in
        kwd
      with Not_found ->
        (Grammar.STRING input)
      end
    }
  | _
    { raise (SyntaxError ("Unexpected char: " ^ lexeme lexbuf)) }
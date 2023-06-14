%{ open Expr %}
%{ open TestImpl %}
%{ open MyTest %}

%token L_PARENS
%token R_PARENS
%token SEMICOLON
%token AND
%token OR
%token NOT
%token <string> STRING

%token TESTB
%token TESTC
%token TESTD
%token B0
%token B1

%token IF
%token THEN
%token ELSE
%token WHILE
%token DO
%token ASSERT

%start <(string, MyTest.t) expr> expr

%%

test:
  | b = primtest { b }
  | b1 = primtest; AND; b2 = test { BAnd(b1, b2) }
  | b1 = primtest; OR; b2 = test {BOr(b1, b2) }

primtest:
  | B0 { B0 }
  | B1 { B1 }
  | TESTB { BTest(B) }
  | TESTC { BTest(C) }
  | TESTD { BTest(D) }
  | NOT; b = primtest { BNot(b) }
  | L_PARENS; b = test; R_PARENS { b }

expr:
  | s = STRING { Do(s) }
  | ASSERT; L_PARENS; b = test; R_PARENS { Assert(b) }
  | L_PARENS; e1 = expr; SEMICOLON; e2 = expr; R_PARENS
    { Seq(e1, e2) }
  | IF; b = test; THEN; e1 = expr; ELSE; e2 = expr 
    { If(b, e1, e2) }
  | WHILE; b = test; DO; e = expr; 
    { While(b, e) }
  | L_PARENS; e = expr; R_PARENS
    { e }




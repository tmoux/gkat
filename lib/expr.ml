type 't bexpr =
  | B0
  | B1
  | BTest of 't
  | BAnd of 't bexpr * 't bexpr
  | BOr of 't bexpr * 't bexpr
  | BNot of 't bexpr

let rec show_bexpr f = function
  | B0 -> "0"
  | B1 -> "1"
  | BTest(t) -> f t
  | BAnd(e, e') -> Format.sprintf "(%s && %s)" (show_bexpr f e) (show_bexpr f e')
  | BOr(e, e') -> Format.sprintf "(%s || %s)" (show_bexpr f e) (show_bexpr f e')
  | BNot(e) -> Format.sprintf "!(%s)" (show_bexpr f e)


type ('a, 't) expr =
  | Do of 'a
  | Assert of 't bexpr
  | Seq of ('a, 't) expr * ('a, 't) expr
  | If of 't bexpr * ('a, 't) expr * ('a, 't) expr
  | While of 't bexpr * ('a, 't) expr

let rec show_expr f g = function
  | Do(x) -> f x
  | Assert(x) -> Format.sprintf "assert (%s)" (g x)
  | Seq(e, e') -> Format.sprintf "(%s; %s)" (show_expr f g e) (show_expr f g e')
  | If(b, e, e') -> Format.sprintf "if %s then (%s) else (%s)" (g b) (show_expr f g e) (show_expr f g e')
  | While(b, e) -> Format.sprintf "while %s do (%s)" (g b) (show_expr f g e)

module type TEST = sig
  type t
  type atom

  val entails : atom -> t bexpr -> bool
  val getAtoms : atom Seq.t
end

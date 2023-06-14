open Expr
open Transition

type ('a, 'at) guarded_string =
  | End of 'at
  | Cons of 'at * 'a * ('a, 'at) guarded_string

module type AUTOMATON = sig
  type atom
  type test
  type s
  type 'action t

  val initState : 'action t -> s
  val trans : 'action t -> s -> atom -> ('action, s) transition
  val from_expr : ('action, test) expr -> 'action t
  val reify : 'action t -> 'action Bisimulation.normalized_automaton
end

module AutomatonExn (A : AUTOMATON) = struct
  include A

  let rec acceptState f st = function
    | End x -> ( match f st x with Accept -> true | _ -> false)
    | Cons (x, p, rest) -> (
        match f st x with
        | Next (p', t) -> p = p' && acceptState f t rest
        | _ -> false)

  let accepts aut = acceptState (trans aut) (initState aut)

  let equiv e f =
    let a = reify (from_expr e) and b = reify (from_expr f) in
    Bisimulation.decide_equivalence a b
end

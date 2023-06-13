open Map
open Expr
open Automaton
open Bisimulation

module MyTest : TEST with type t = string and type atom = bool list = struct
  type t = string
  type atom = bool list

  let rec entails at = function
    | B0 -> false
    | B1 -> true
    | BTest t -> if t = "b" then List.nth at 0 else List.nth at 1
    | BAnd (b, c) -> entails at b && entails at c
    | BOr (b, c) -> entails at b || entails at c
    | BNot b -> not (entails at b)

  let getAtoms =
    List.to_seq
      [ [ false; false ]; [ false; true ]; [ true; false ]; [ true; true ] ]

  (* let show_t t = t
     let show_atom ats = (String.concat " " (List.map (fun x -> if x then "1" else "0") ats)) *)
end

module MyOrd : OrderedType with type t = MyTest.atom = struct
  type t = MyTest.atom

  let compare = compare
end

module MyAutomaton = AutomatonExn (IntAutomaton.IntAutomaton (MyTest) (MyOrd))

let e_b = Assert (BTest "b")
let e_p = Do "p"
let e_if = If (BTest "b", Do "p", Do "q")
let e_seq = Seq (Do "p", Do "q")
let e = While (BTest "b", Do "p")
let f = If (BTest "c", Do "q", Do "r")
let e_t = Seq (e, f)
let aut = MyAutomaton.from_expr e_t
let norm = MyAutomaton.reify aut
let () = normalize norm
let should_be_t = decide_equivalence norm norm
let should_be_t2 = MyAutomaton.equiv e_t (Seq (e_t, Assert B1))

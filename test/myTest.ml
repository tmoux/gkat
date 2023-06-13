open Gkatlib.Expr

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
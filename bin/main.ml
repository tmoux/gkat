open Gkatlib.Expr
open Map

module MyTest : TEST = struct
  type t = string
  type atom = bool list

  let rec entails at = function
    | B0 -> false
    | B1 -> true
    | BTest t -> if t == "b" then List.nth at 0 else List.nth at 1
    | BAnd(b, c) -> entails at b && entails at c
    | BOr(b, c) -> entails at b || entails at c
    | BNot(b) -> not (entails at b)

  let getAtoms = List.to_seq [[false; false]; [false; true]; [true; false]; [true; true]]


  (* let show_t t = t
  let show_atom ats = (String.concat " " (List.map string_of_bool ats)) *)
end

module MyOrd : OrderedType with type t = MyTest.atom = struct
  type t = MyTest.atom
  let compare = compare
end

let () =
  print_endline "Hello, World!";
  print_endline asdf

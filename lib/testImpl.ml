open Expr

module MyTest = struct
  type t = B | C | D
  type atom = t -> bool

  let rec entails at = function
    | B0 -> false
    | B1 -> true
    | BTest t -> at t
    | BAnd (b, c) -> entails at b && entails at c
    | BOr (b, c) -> entails at b || entails at c
    | BNot b -> not (entails at b)

  let getAtoms =
    List.to_seq
      [
        (function B -> false | C -> false | D -> false);
        (function B -> false | C -> false | D -> true);
        (function B -> false | C -> true | D -> false);
        (function B -> false | C -> true | D -> true);
        (function B -> true | C -> false | D -> false);
        (function B -> true | C -> false | D -> true);
        (function B -> true | C -> true | D -> false);
        (function B -> true | C -> true | D -> true);
      ]

  let show_t = function
    | B -> "b"
    | C -> "c"
    | D -> "d"
end

module MyTestOrd : Map.OrderedType with type t = MyTest.atom = struct
  open MyTest

  type t = MyTest.atom

  let compare x y =
    let f x = [ x B; x C; x D ] in
    compare (f x) (f y)
end

let my_show_expr = show_expr (fun x -> x) (show_bexpr MyTest.show_t) 
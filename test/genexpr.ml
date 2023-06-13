open Gkatlib.Expr


let btest x = BTest x
let band x y = BAnd (x, y)
let bor x y = BOr (x, y)
let bnot x = BNot x
let gent = QCheck.Gen.oneofl [ "b"; "c" ]

let gentest sz (gent : 't QCheck.Gen.t) : 't bexpr QCheck.Gen.t =
  let open QCheck.Gen in
  sized_size sz
  @@ fix (fun self n ->
         match n with
         | 0 ->
             frequency
               [
                 (1, return B0); (1, return B1); (4, map (fun x -> BTest x) gent);
               ]
         | n ->
             frequency
               [
                 (1, return B0);
                 (1, return B1);
                 (4, map (fun x -> BTest x) gent);
                 (8, map2 band (self (n / 2)) (self (n / 2)));
                 (8, map2 bor (self (n / 2)) (self (n / 2)));
                 (8, map bnot (self (n / 2)));
               ])

let genexpr sz genac gentest =
  let open QCheck.Gen in
  sized_size sz
  @@ fix (fun self n ->
         match n with
         | 0 -> oneof [ map (fun x -> Do x) genac; map (fun x -> Assert x) gentest ]
         | n ->
             frequency
               [
                 (1, map (fun x -> Do x) genac);
                 (1, map (fun x -> Assert x) gentest);
                 (4, map2 (fun x y -> Seq (x, y)) (self (n / 2)) (self (n / 2)));
                 ( 4,
                   map3
                     (fun x y z -> If (x, y, z))
                     gentest
                     (self (n / 2))
                     (self (n / 2)) );
                 (4, map2 (fun x y -> While (x, y)) gentest (self (n / 2)));
               ])

(* The type of actions doesn't really matter, so let's just pick our set of actions to be one-letter strings since they're easy to print: *)
let genac = QCheck.Gen.oneofl ["p"; "q"; "r"; "s"; "t"; "u"]

let arbitrary_test =
  let open QCheck.Gen in
  let sz_test = 15 in
  QCheck.make (gentest (int_bound sz_test) gent)

let arbitrary_expr = 
  let open QCheck.Gen in
  let sz_expr = 20 and sz_test = 20 in
  QCheck.make (genexpr (int_bound sz_expr) genac (gentest (int_bound sz_test) gent))
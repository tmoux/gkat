open OUnit2
open Axioms
open Genexpr

let make_test label arb f =
  QCheck_ounit.to_ounit2_test (QCheck.Test.make ~count:100 ~long_factor: 2 ~name:label arb f)

let axiom_tests =
  let open QCheck in
  let e = arbitrary_expr and b = arbitrary_test in
  [
    make_test "U1" (tup2 e b) u1;
    make_test "U2" (tup3 e e b) u2;
    make_test "U3" (tup5 e e e b b) u3;
    make_test "U4" (tup3 e e b) u4;

    make_test "U5" (tup4 e e e b) u5;
    make_test "S1" (tup3 e e e) s1;
    make_test "S2" e s2;
    make_test "S3" e s3;
    make_test "S4" e s4;
    make_test "S5" e s5;

    make_test "W1" (tup2 e b) w1;
    make_test "W2" (tup3 e b b) w2;
    make_test "W3" (tup4 e e e b) w3;
  ]

let tests = "test" >::: axiom_tests

(*
let ls = QCheck.Gen.generate ~n:20 (gentest (QCheck.Gen.return 10) gent)
let _ = () (* print_string (String.concat "\n\n" (List.map (show_bexpr (fun x -> x)) ls)) *)

let es = let open QCheck.Gen in generate ~n:20 (genexpr (int_bound 15) genac (gentest (int_bound 15) gent))
let _ = print_string (String.concat "\n\n" (List.map (show_expr (fun x -> x) (show_bexpr (fun x -> x))) es))
*)

let () = run_test_tt_main tests

(*
  TODO:
  - Use qcheck to generate expressions/btests.
  - Use this instead to generate tests for the axioms.
  - We should also check congruence rules. To do this, let's start with a random expression, randomly apply rewrites to each side, then apply congruence to each side to check if they are still equal.
  - This helps show completeness, but soundness not so much.
  - To handle this, we should check that if two expression have equivalent automata, then they are equivalent by the axioms.
  - This is tricker; we need an oracle to determine whether two expressions are equivalent by the axioms (egraphs?)
  - On the other hand, we could pick some expressions that are obviously not equivalent, and show that they are not decided to be equivalent.

  - Implement pointer implementation, and check against int implementation
*)
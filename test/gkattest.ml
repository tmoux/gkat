open OUnit2
open Axioms
open Genexpr
open WeakestTest
open Gkat.IntAutomaton
open Gkat.Automaton
open Gkat.TestImpl
module MyAutomaton = AutomatonExn (IntAutomaton (MyTest) (MyTestOrd))
module MyWeakestTest = WeakestTest (MyTest)

let tests =
  "testInt"
  >::: axiom_tests MyAutomaton.equiv MyWeakestTest.isProductive arbitrary_expr
         arbitrary_test

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

  - Add a parser
  - General reorganizing stuff
  - Implement pointer implementation, and check against int implementation
*)

open Gkatlib.Expr
open Gkatlib.IntAutomaton
open Gkatlib.Automaton
open WeakestTest


module MyOrd : Map.OrderedType with type t = MyTest.atom = struct
  type t = MyTest.atom

  let compare = compare
end

module MyAutomaton = AutomatonExn (IntAutomaton (MyTest) (MyOrd))

module MyWeakestTest = WeakestTest(MyTest)
let e1 = Assert B1
let e0 = Assert B0

open MyAutomaton

(* Idempotence: e = e +_b e *)
let u1 (e, b) = equiv e (If (b, e, e))

(* Skew commutativity: e +_b f = f +_~b e *)
let u2 (e, f, b) = equiv (If (b, e, f)) (If (BNot b, f, e))

(* Skew associativity: (e +_b f) +_c g = e +_bc (f +_c g) *)
let u3 (e, f, g, b, c) =
  equiv (If (c, If (b, e, f), g)) (If (BAnd (b, c), e, If (c, f, g)))

(* Guardedness: e +_b f = be +_b f *)
let u4 (e, f, b) = equiv (If (b, e, f)) (If (b, Seq (Assert b, e), f))

(* Right distributivity: eg +_b fg = (e +_b f) g *)
let u5 (e, f, g, b) =
  equiv (If (b, Seq (e, g), Seq (f, g))) (Seq (If (b, e, f), g))

(* Sequence associativity *)
let s1 (e, f, g) = equiv (Seq (e, Seq (f, g))) (Seq (Seq (e, f), g))

(* 0 is an annihilator for left/right sequencing *)
let s2 e = equiv (Seq (e0, e)) e0
let s3 e = equiv (Seq (e, e0)) e0

(* 1 is a identity for left/right sequencing *)
let s4 e = equiv (Seq (e1, e)) e
let s5 e = equiv (Seq (e, e1)) e

(* Loop unrolling: e^b = ee^b +_b 1 *)
let w1 (e, b) = equiv (While (b, e)) (If (b, Seq (e, While (b, e)), e1))

(* Loop tightening: *)
let w2 (e, b, c) =
  equiv (While (b, If (c, e, e1))) (While (b, Seq (Assert c, e)))

(* Loop fixpoint: *)
let w3 (e, f, g, b) =
  QCheck.assume (MyWeakestTest.isProductive e);
  QCheck.assume (equiv g (If (b, Seq (e, g), f)));
  equiv g (Seq (While (b, e), f))

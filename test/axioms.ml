open Gkat.Expr


let e1 = Assert B1
let e0 = Assert B0

(* Idempotence: e = e +_b e *)
let u1 equiv (e, b) = equiv e (If (b, e, e))

(* Skew commutativity: e +_b f = f +_~b e *)
let u2 equiv (e, f, b) = equiv (If (b, e, f)) (If (BNot b, f, e))

(* Skew associativity: (e +_b f) +_c g = e +_bc (f +_c g) *)
let u3 equiv (e, f, g, b, c) =
  equiv (If (c, If (b, e, f), g)) (If (BAnd (b, c), e, If (c, f, g)))

(* Guardedness: e +_b f = be +_b f *)
let u4 equiv (e, f, b) = equiv (If (b, e, f)) (If (b, Seq (Assert b, e), f))

(* Right distributivity: eg +_b fg = (e +_b f) g *)
let u5 equiv (e, f, g, b) =
  equiv (If (b, Seq (e, g), Seq (f, g))) (Seq (If (b, e, f), g))

(* Sequence associativity *)
let s1 equiv (e, f, g) = equiv (Seq (e, Seq (f, g))) (Seq (Seq (e, f), g))

(* 0 is an annihilator for left/right sequencing *)
let s2 equiv e = equiv (Seq (e0, e)) e0
let s3 equiv e = equiv (Seq (e, e0)) e0

(* 1 is a identity for left/right sequencing *)
let s4 equiv e = equiv (Seq (e1, e)) e
let s5 equiv e = equiv (Seq (e, e1)) e

(* Loop unrolling: e^b = ee^b +_b 1 *)
let w1 equiv (e, b) = equiv (While (b, e)) (If (b, Seq (e, While (b, e)), e1))

(* Loop tightening: *)
let w2 equiv (e, b, c) =
  equiv (While (b, If (c, e, e1))) (While (b, Seq (Assert c, e)))

(* Loop fixpoint: *)
let w3 equiv is_productive (e, f, g, b) =
  QCheck.assume (is_productive e);
  QCheck.assume (equiv g (If (b, Seq (e, g), f)));
  equiv g (Seq (While (b, e), f))

let make_test label arb f =
  QCheck_ounit.to_ounit2_test
    (QCheck.Test.make ~count:100 ~long_factor:2 ~name:label arb f)

let axiom_tests f p e b =
  let open QCheck in
  [
    make_test "U1" (tup2 e b) (u1 f);
    make_test "U2" (tup3 e e b) (u2 f);
    make_test "U3" (tup5 e e e b b) (u3 f);
    make_test "U4" (tup3 e e b) (u4 f);
    make_test "U5" (tup4 e e e b) (u5 f);
    make_test "S1" (tup3 e e e) (s1 f);
    make_test "S2" e (s2 f);
    make_test "S3" e (s3 f);
    make_test "S4" e (s4 f);
    make_test "S5" e (s5 f);
    make_test "W1" (tup2 e b) (w1 f);
    make_test "W2" (tup3 e b b) (w2 f);
    make_test "W3" (tup4 e e e b) (w3 f p);
  ]

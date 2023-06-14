open Gkat.Expr

module WeakestTest(T : TEST) = struct
  open T
  let rec weakest_test (e : ('action, t) expr) : 't bexpr = match e with
    | Do(_) -> B0
    | Assert(b) -> b
    | If(b, e, f) -> BOr(BAnd(b, weakest_test e), BAnd(BNot(b), weakest_test f))
    | Seq(e, f) -> BAnd(weakest_test e, weakest_test f)
    | While(b, _) -> BNot(b)
  
  let isZero b = Seq.for_all (fun at -> not (entails at b)) getAtoms
  let isProductive e = isZero (weakest_test e)
end
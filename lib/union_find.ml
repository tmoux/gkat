(* UF data structure using union-by-size merging and path compression *)
type t = int array

let init n = Array.make n (-1)

let rec find i uf =
  let p = uf.(i) in
  if p < 0 then i
  else (
    uf.(i) <- find p uf;
    uf.(i))

let union i j uf = let x = find i uf and y = find j uf in
  if x = y then ()
  else let merge a b = uf.(a) <- uf.(a) + uf.(b); uf.(b) <- a in
    if uf.(x) > uf.(y) then merge x y else merge y x

let same_component i j uf = find i uf = find j uf
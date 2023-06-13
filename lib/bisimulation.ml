open Transition

type 'action normalized_automaton = ('action, int) transition list array

let normalize aut =
  let n = Array.length aut in
  let live = Array.make n false in
  let has_accept l =
    Option.is_some (List.find_opt (function Accept -> true | _ -> false) l)
  in
  let has_trans l j =
    Option.is_some
      (List.find_opt (function Next (_, s) -> s = j | _ -> false) l)
  in
  let mark init =
    let queue = Queue.create () in
    let rec bfs () =
      if Queue.is_empty queue then ()
      else
        let f = Queue.pop queue in
        (* This is a bit inefficient; instead, we should build an adjacency list with reversed edges. *)
        let rec iter i : unit =
          if i < 0 then ()
          else (
            if has_trans aut.(i) f && not live.(i) then (
              live.(i) <- true;
              Queue.add i queue)
            else ();
            iter (i - 1))
        in
        iter (n - 1);
        bfs ()
    in
    live.(init) <- true;
    Queue.add init queue;
    bfs ()
  in
  let rec loop i =
    if i < 0 then ()
    else (
      if has_accept aut.(i) then mark i else ();
      loop (i - 1))
  in
  loop (n - 1);
  let update_trans = function
    | Accept -> Accept
    | Reject -> Reject
    | Next (p, s) -> if live.(s) then Next (p, s) else Reject
  in
  let rec modify i : unit =
    if i < 0 then ()
    else (
      aut.(i) <- List.map update_trans aut.(i);
      modify (i - 1))
  in
  modify (n - 1)

let decide_equivalence a b =
  normalize a; normalize b;
  let n = max (Array.length a) (Array.length b) in
  let dsu = Union_find.init (2 * n) in
  let queue = Queue.create () in
  let rec loop () =
    if Queue.is_empty queue then true
    else
      let x, y = Queue.take queue in
      let rx = Union_find.find x dsu and ry = Union_find.find (y + n) dsu in
      if rx = ry then loop ()
      else
        let rec add_next xs ys =
          match (xs, ys) with
          | tx :: restx, ty :: resty -> (
              match (tx, ty) with
              | Accept, Accept -> add_next restx resty
              | Reject, Reject -> add_next restx resty
              | Next (px, nx), Next (py, ny) ->
                  if px = py then (
                    Queue.add (nx, ny) queue;
                    add_next restx resty)
                  else false
              | _ -> false)
          | _ -> true
        in
        Union_find.union x (y + n) dsu;
        if add_next a.(x) b.(y) then loop () else false
  in
  Queue.add (0, 0) queue;
  loop ()

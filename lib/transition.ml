type ('a, 's) transition = Accept | Reject | Next of 'a * 's

let show_transition f g = function
  | Accept -> "Accept"
  | Reject -> "Reject"
  | Next (a, s) -> f a ^ ", " ^ g s
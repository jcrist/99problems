(* Problem 1: Find the last element of a list *)

let rec last ls =
  match ls with
  | [] -> None
  | [tl] -> Some tl
  | _ :: tl -> last tl

(* Tests *)
assert (last [0] = Some 0)
assert (last [0; 1; 2] = Some 2)
assert (last [] = None)

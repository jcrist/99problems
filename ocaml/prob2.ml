(* Problem 2: Find the last but one element of a list *)

let rec penultimate ls =
    match ls with
    | [] | [_] -> None
    | [hd; _] -> Some hd
    | _ :: tl -> penultimate tl

(* Tests *)
assert (penultimate [] = None)
assert (penultimate [1] = None)
assert (penultimate [1; 2] = Some 1)
assert (penultimate [1; 2; 3] = Some 2)

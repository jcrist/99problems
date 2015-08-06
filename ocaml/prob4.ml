(* Find the number of elements in a list *)

let length ls =
    let rec _length ls cnt =
        match ls with
        | [] -> cnt
        | hd :: tl -> _length tl cnt + 1
    in _length ls 0
;;

(* Tests *)
assert (length [] = 0)
assert (length [1] = 1)
assert (length [1; 2] = 2)
assert (length [1; 2; 3] = 3)

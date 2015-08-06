(* Reverse a list *)

let reverse ls =
    let rec _reverse rev ls =
        match ls with
        | [] -> rev
        | hd :: tl -> _reverse (hd :: rev) tl
    in _reverse [] ls
;;

(* Tests *)
assert (reverse [] = [])
assert (reverse [1] = [1])
assert (reverse [1; 2] = [2; 1])
assert (reverse [1; 2; 3] = [3; 2; 1])

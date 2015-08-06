(* Find out whether a list is a palindrome *)

let is_palindrome ls =
    let rec _is_palindrome rev ls =
        match ls with
        | [] -> rev
        | hd :: tl -> _is_palindrome (hd :: rev) tl
    in _is_palindrome [] ls = ls
;;

(* Tests *)
assert (is_palindrome [] = true)
assert (is_palindrome [1] = true)
assert (is_palindrome [1; 2] = false)
assert (is_palindrome [1; 2; 1] = true)

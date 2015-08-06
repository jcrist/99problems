(* Find the k'th element of a list: The first element in the list is number 1 *)

let rec getitem ls k =
    match ls with
    | [] -> None
    | hd :: tl -> if k = 1 then Some hd else getitem tl (k - 1)
;;

(* Tests *)
let ls = [1;2;3]
assert (getitem ls 0 = None)
assert (getitem ls 1 = Some 1)
assert (getitem ls 2 = Some 2)
assert (getitem ls 3 = Some 3)
assert (getitem ls 4 = None)

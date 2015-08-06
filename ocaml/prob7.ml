type 'a node =
    | One of 'a
    | Many of 'a node list;;

let flatten node =
    let rec _flatten node accum =
        match node with
        | [] -> accum
        | One hd :: tl -> _flatten tl (hd :: accum)
        | Many hd :: tl -> _flatten tl (_flatten hd accum)
    in let reverse ls =
        let rec _reverse rev ls =
            match ls with
            | [] -> rev
            | hd :: tl -> _reverse (hd :: rev) tl
        in _reverse [] ls
    in reverse (_flatten node [])
;;

(* Tests *)
assert (flatten [One 1; Many[One 2; Many[One 3; One 4]; One 5]] =
    [1; 2; 3; 4; 5])
assert (flatten [One 1] = [1])
assert (flatten [] = [])
assert (flatten [One 1; One 2; One 3] = [1; 2; 3])

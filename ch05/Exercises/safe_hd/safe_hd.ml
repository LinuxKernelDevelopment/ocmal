let safe_hd = function
        | h :: t -> Some h
        | [] -> None

let rec safe_tl = function
        | h1 :: t1 -> (match t1 with
                            | [] -> Some h1
                            | _ -> safe_tl t1)
        | [] -> None

let bigred = function
    | [] -> false
    | h :: t -> h = "bigred"

let twoFour = function
    | h1 :: h2 :: t -> ( match t with
                             | [] -> true
                             | t1 :: t2 :: t3 -> (match t3 with
                                                    | [] -> true
                                                    | _ -> false)
                             | _ -> false


let ftequal = function
    | fst :: snd :: t -> fst = snd
    | _ -> false

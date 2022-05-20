
let lst = [ (1, "one"); (2, "two"); (3, "three") ]

let insert k v lst = (k, v) :: lst

let rec lookup k = function
        | [] -> None
        | (k', v) :: t -> if k = k' then Some v else lookup k t

let main =
        match (lookup 2 lst) with
                | None -> ""
                | Some y -> y

let a = main  

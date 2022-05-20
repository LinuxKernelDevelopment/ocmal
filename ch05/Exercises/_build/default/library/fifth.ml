let fifth (lst : int list) = match lst with
    | _ :: _ :: _ :: _ :: h5 :: _ -> h5
    | lst ->if List.length lst < 5 then 0 else failwith "Not possible"

let lstDec = function
    | lst -> List.sort Stdlib.compare lst |> List.rev

let rec ( -- ) i j = if i > j then [] else i :: i + 1 -- j

let sum_cube_odd n =
        List.fold_left ( + ) 0 (List.map (fun x -> x * x * x) (List.filter (fun x -> (x mod 2) == 1) (1 -- n)))

let sum_cube_odd_pipeline n =
        1 -- n
        |> List.filter (fun x -> (x mod 2) == 1)
        |> List.map (fun x -> x * x * x)
        |> List.fold_left ( + ) 0

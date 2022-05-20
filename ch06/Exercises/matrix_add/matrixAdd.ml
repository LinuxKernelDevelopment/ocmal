let add_matrix (m1 : int list list) (m2 : int list list) =
        List.map2 (fun a b -> List.map2 (fun a1 b1 -> a1 + b1) a b) m1 m2

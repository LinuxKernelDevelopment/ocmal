let product_left (x : float list) =
        List.fold_left ( *. ) 1.0 x

let product_right (x : float list) =
        List.fold_right ( *. ) x 1.0

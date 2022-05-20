let thrd t = match t with x, y, z -> z

let thrd t =
        let x, y, z = t in
        z

let thrd t =
        let _, _, z = t in
        z

let thrd (_, _, z) = z

let rec exists_rec p = function
        | h :: t -> if p h then true else exists_rec p t
        | [] -> false

let exists_fold p lst =
        List.fold_left (fun b x -> b || (p x)) false lst

let exists_lib p lst =
        List.filter p lst != []

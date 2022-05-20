let key lst = List.sort_uniq (fun x y -> x - y) (fst (List.split lst))

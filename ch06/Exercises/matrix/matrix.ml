let is_valid_matrix lst = match lst with
        | [] -> false
        | h :: t -> ( match (List.filter (fun x -> List.length x != List.length h) t) with
                           | _ :: _ -> false
                           | [] -> true 
                        )

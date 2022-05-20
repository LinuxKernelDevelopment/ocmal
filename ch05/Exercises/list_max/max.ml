let list_max lst =
    let rec list_max_aux lst num = match lst with
          | [] -> num
          | h :: t -> if h > num then list_max_aux t h else list_max_aux t num
    in
        match lst with
          | [] -> failwith "list_max"
          | h :: t -> list_max_aux t h

let list_max lst =
    let rec list_max_aux lst num = match lst with
          | [] -> string_of_int num
          | h :: t -> if h > num then list_max_aux t h else list_max_aux t num
    in
        match lst with
          | [] -> "empty"
          | h :: t -> list_max_aux t h

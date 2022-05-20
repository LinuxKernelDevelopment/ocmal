let matrix_transposition (lst : int list list) = match lst with
           | h :: t -> begin
                          let aux_list = List.init (List.length h) Fun.id in
                          let rec matrix_transposition_aux aux_lst lst = match aux_lst with
                                  | h :: t -> (List.map (fun a -> List.nth a h) lst) ::  (matrix_transposition_aux t lst)
                                  | _ -> [] in
                          matrix_transposition_aux aux_list lst
                       end
           | _ -> []

let rec multiply_matrices_aux lst1 lst2 =
          let multiply_matrices_one_element lst1 lst2tl1 =
                List.fold_left (fun a b -> a + b) 0 (List.map2 (fun a b -> a * b) lst1 lst2tl1) in
          match lst2 with
              | h :: t -> (multiply_matrices_one_element lst1 h) :: (multiply_matrices_aux lst1 t)
              | [] -> []

let rec multiply_matrices_aux2 (lst1 : int list list) (lst2t : int list list) =
    match lst1 with
        | h :: t -> multiply_matrices_aux h lst2t :: multiply_matrices_aux2 t lst2t  
        | [] -> []

let multiply_matrices (lst1: int list list) (lst2 : int list list) =
    multiply_matrices_aux2 lst1 (matrix_transposition lst2)

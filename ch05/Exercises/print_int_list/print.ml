let rec print_int_list = function
      | [] -> ()
      | h :: t -> Printf.printf "%d\n" h; print_int_list t

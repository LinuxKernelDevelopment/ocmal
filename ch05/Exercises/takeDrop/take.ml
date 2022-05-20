let rec take n lst = match lst
        | h :: t -> if n > 0 h :: take (n - 1) t else []
        | [] -> []

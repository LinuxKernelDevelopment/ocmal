let rec drop n lst = match lst with
        | [] -> []
        | h :: t -> if n > 1 then drop (n - 1) t else t

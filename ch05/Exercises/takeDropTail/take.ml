let take_tr n lst =
        let rec take_aux n lst acc = match lst with
                | h :: t -> if n > 0 then take_aux (n - 1) t (acc @ [h]) else acc
                | [] ->[] in
        take_aux n lst []


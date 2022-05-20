let rec concate = function
    | [] -> ""
    | h :: t -> h ^ concate t

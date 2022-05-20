let rec mulList = function
    | [] -> 1
    | h :: t -> h * mulList t;;

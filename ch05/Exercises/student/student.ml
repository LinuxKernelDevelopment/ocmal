type student = {first_name : string; last_name : string; gpa : float}

let get_name (s : student) =
        s.last_name ^ " " ^ s.first_name

let student first_name last_name gpa = { first_name = first_name; last_name = last_name; gpa = gpa }

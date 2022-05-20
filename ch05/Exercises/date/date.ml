let month_day m leap = match m with                                                                                                                   
| 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31                                                                                                     
| 2 -> if leap then 29 else 28                                                                                                          
| 4 | 6 | 9 | 11 -> 30                                                                                                                  
| _ -> failwith "Illegal";;
let check_date date = match date with y, m, d -> if d > month_day m ((y mod 4) = 0) || d < 0 then failwith "Illegal day";;
let before a b = match a with ay, am, ad ->
                  match b with by, bm, bd -> check_date a; check_date b; ay < by || (ay = by && am < bm) || (ay = by && am = bm && ad < bd);;

let date_list = [ (1970, 1, 1); (2000, 1, 1); (1989, 9, 10); (2022, 3, 19)]

let earlist lst =
        let rec earlist_help lst d = match lst with
                        | [] -> d
                        | h :: t -> if (before h d) then (earlist_help t h) else (earlist_help t d) in
        match lst with
        | [] -> None
        | h :: t -> Some (earlist_help t h);;




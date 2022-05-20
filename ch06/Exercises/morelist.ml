let slen x = List.filter (fun s -> String.length s > 3) x

let add1 x = List.map (fun x -> x +. 1.0) x

let concat slst = List.fold_left (fun x y -> if (not (String.equal x "")) then x ^ ";" ^ y else y) "" slst

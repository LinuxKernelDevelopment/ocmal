let rec increase = function
        | h1 :: t1 -> (match t1 with
                          | h2 :: t2 -> if h1 < h2 then increase t1 else false
                          | [] -> true)
        | [] -> true

let rec decrease = function
        | h1 :: t1 -> (match t1 with
                          | h2 :: t2 -> if h1 > h2 then decrease t1 else false
                          | [] -> true)
        | [] -> true

let rec unimodalRemain lst = match lst with
             | h1 :: t1 -> (match t1 with
                               | h2 :: t2 -> (if h1 < h2 then unimodal t1
                                             else t1)
                               | [] -> [])
             | [] -> []

let unimodal lst =
        let rec decrease = function
                | h1 :: t1 -> (match t1 with
                                   | h2 :: t2 -> if h1 > h2 then decrease t1 else false
                                   | [] -> true)
                | [] -> true in
        let rec unimodalRemain = function
                | h1 :: t1 -> (match t1 with
                                     | h2 :: t2 -> (if h1 < h2 then unimodalRemain t1
                                                    else t1)
                                     | [] -> [])
                | [] -> [] in
        decrease (unimodalRemain lst);;

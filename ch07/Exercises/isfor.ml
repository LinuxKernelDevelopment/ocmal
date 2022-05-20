module CharMap = Map.Make (Char)
let cm = CharMap.(empty |> add 'a' "apple")

let dm = CharMap.mapi (fun a b-> Char.escaped(a) ^ " is for " ^ b) cm

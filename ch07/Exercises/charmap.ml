module CharMap = Map.Make(Char)

let k1 = 'A'
let k2 = 'E'
let k3 = 'S'
let k4 = 'V'

open CharMap

let cm = empty
let cm = cm |> add k1 "Alpha" |> add k2 "Echo" |> add k3 "Sierra" |> add k4 "Victor" 

let _ = CharMap.find 'A' cm

let _ = CharMap.mem 'A' cm

let _ = CharMap.bindings cm

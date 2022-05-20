type quad = I | II | III | IV
type sign = Neg | Zero | Pos
let sign (x:int) : sign = if (x > 0) then Pos else if (x < 0) then Neg else Zero

let quadrant : int*int -> quad option = fun (x, y) ->
    match (x, y) with
        | (x, y) when x > 0 && y > 0 -> Some I
        | (x, y) when x < 0 && y > 0 -> Some II
        | (x, y) when x < 0 && y < 0 -> Some III
        | (x, y) when x > 0 && y < 0 -> Some IV
        | _ -> None


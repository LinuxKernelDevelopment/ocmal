let plus_opt (x:int option) (y:int option) : int option =
  match x, y with
  | None, _ | _, None -> None
  | Some a, Some b -> Some (Stdlib.( + )  a b)

let ( + ) = plus_opt

let minus_opt (x:int option) (y:int option) : int option =
  match x, y with
  | None, _ | _, None -> None
  | Some a, Some b -> Some (Stdlib.( - ) a b)

let ( - ) = minus_opt

let mult_opt (x:int option) (y:int option) : int option =
  match x, y with
  | None, _ | _, None -> None
  | Some a, Some b -> Some (Stdlib.( * ) a b)

let ( * ) = mult_opt

let div_opt (x:int option) (y:int option) : int option =
  match x,y with
  | None, _ | _, None -> None
  | Some a, Some b ->
    if b = 0 then None else Some (Stdlib.( / ) a b)

let ( / ) = div_opt
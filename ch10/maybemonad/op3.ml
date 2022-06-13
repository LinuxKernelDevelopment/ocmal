let propagate_none
  (op : int -> int -> int option) (x : int option) (y : int option)
=
  match x, y with
  | None, _ | _, None -> None
  | Some a, Some b -> op a b

let div (x:int) (y:int) : int option =
  if y = 0 then None else Some (x / y)

let ( / ) = propagate_none div
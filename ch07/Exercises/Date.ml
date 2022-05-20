type date = { month : int; day: int }

module Date = struct
  type t = date
  let compare { month = month1; day = day1 } {month = month2; day = day2}
      =
      match month1 < month2 with
        | true -> -1
        | false -> (if month1 = month2 && day1 = day2 then 0 else 1)
end

let k1 = {month = 3; day = 31} and k2 = {month = 2; day = 29};;
let k3 = {month = 12; day = 21} and k4 = {month = 9; day = 10};;

module DateMap = Map.Make (Date)
let dm = DateMap.(empty |> add k1 1 |> add k2 2 |> add k3 12 |> add k4 88)

type calendar = string DateMap.t

let sm : calendar = DateMap.(empty |> add k1 "fuck")

let print_calendar {month=m1; day=d1} value = Printf.printf "%d.%d %s\n" m1 d1 value

let _ = DateMap.iter print_calendar sm

let first_after (c : calendar) (t : Date.t) = DateMap.find t c

let x = first_after sm k1


(* module SetIns = struct
  module SS = Set.Make(String)
  let set = SS.empty
  let uset = SS.empty

  let add (s : string) =
          let us = Stdlib.String.uppercase_ascii(s) in
          if SS.mem us uset then set else SS.add s set; SS.add us uset; set

  let remove (s : string) =
          let us = Stdlib.String.uppercase_ascii(s) in
          if SS.mem us uset then (SS.remove us uset; SS.filter (fun elt -> if us = Stdlib.String.uppercase_ascii(elt) then false else true) set) else set
end *)

module StringIns = struct
  type t = string
  let compare s1 s2 = Stdlib.compare (Stdlib.String.uppercase_ascii(s1)) (Stdlib.String.uppercase_ascii(s2))
end

module StringInsSet = Set.Make(StringIns)

let _ = StringInsSet.(empty |> add "grr" |> add "argh" |> add "aRgh" |> add "GRR")

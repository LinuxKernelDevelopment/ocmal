module type Interval = sig
  type t
  val p1 : t
  val p2 : t
  val merge : t -> t -> t
  val remove : t -> t -> t * t
  val rep_ok : t -> t
  val to_string : t -> string
  val pp :
      Format.formatter -> t -> unit
end

module PairInterval : Interval = struct
  type t = int * int 

  let p1 = 1, 5
  let p2 = 2, 6

  let rep_ok interval = match fst interval <= snd interval with
                         | true -> interval
                         | false -> failwith "RI"

  let merge interval1 interval2 = match fst (rep_ok interval1) < fst (rep_ok interval2) with
                                    | true -> ( 
                                              match snd interval1 > snd interval2 with
                                                 | true -> interval1
                                                 | false -> fst interval1, snd interval2
                                               )
                                    | false -> (
                                               match snd interval1 > snd interval2 with
                                                 | true -> fst interval2, snd interval1
                                                 | false -> interval2
                                    )

  let remove interval1 interval2 = if snd (rep_ok interval2) < fst (rep_ok interval1) then
                                      interval1, (0, 0)
                                   else if fst interval2 < fst interval1 then
                                      (fst interval1, snd interval2), (0, 0)
                                   else if snd interval2 < snd interval1 then
                                      (fst interval1, fst interval2), (snd interval2, snd interval1)
                                   else if fst interval2 < snd interval1 then
                                      (fst interval1, fst interval2), (0, 0)
                                   else
                                      interval1, (0, 0)
  
  let to_string interval = Printf.sprintf "%d-%d" (fst (rep_ok interval)) (snd interval)

  let pp fmt interval =
      let open Format in
      fprintf fmt "%s" (to_string interval)

end

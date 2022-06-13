module type Map = sig
  (** [('k, 'v) t] is the type whose key is ['k], value is ['v] *)
  type ('k, 'v) t

  (** [empty] is the empty map*)
  val empty : ('k, 'v) t

  (** [mem 'k ('k, 'v) t] is whether ['k] is one key value of [('k, 'v) t]*)
  val mem : 'k -> ('k, 'v) t -> bool

  (** [find 'k ('k, 'v) t] find the value of corresponding ['k] in [('k, 'v) t] *)
  val find : 'k -> ('k, 'v) t -> 'v

  (** [add ('k, 'v) ('k, 'v) t] add [('k, 'v)] to [('k, 'v) t] *)
  val add : 'k  * 'v -> ('k, 'v) t -> ('k, 'v) t

  (** [remove ('k, 'v) ('k, 'v) t] remove the ('k, 'v) from ('k, 'v) t*)
  val remove : 'k -> ('k, 'v) t -> ('k, 'v) t
end

module ListMap  = struct
  type ('k, 'v) list

  let empty  = []

  let rec mem k m = match m with
                     | h :: t -> if k = (fst h) then true else mem k t
                     | _ -> false

  let rec find key m = match m with
                        | h :: t -> if key = (fst h) then snd h else find key t
                        | _ -> failwith "Not found"

  let add kv m = m @ [kv]

  let remove k m = List.filter (fun e -> k != e) m
  
end
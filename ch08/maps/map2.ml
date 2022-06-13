module Map : sig
  type ('k, 'v) t = 'k -> 'v
  val empty: ('k, 'v) t
  val add: 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  val find: 'k -> ('k, 'v) t -> 'v
  val mem: 'k -> ('k, 'v) t -> bool
  val remove: 'k -> ('k, 'v) t -> ('k, 'v) t
end
= struct
  type ('k, 'v) t = 'k -> 'v

  let empty _ = raise Not_found

  let add k v map =
    fun k' -> if k' = k then v else map k'

  let find (k: 'k) (map: ('k, 'v) t) =
    map k

  let mem k map =
    try
      let _ = map k in
      true
    with Not_found -> false

  let remove k map =
    fun k' -> if k = k' then raise Not_found else map k'
end
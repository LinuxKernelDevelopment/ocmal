module type TableMap = sig
  (** [('k, 'v) t] is the type of mutable table-based maps that bind
      keys of type ['k] to values of type ['v]. *)
  type ('k, 'v) t

  (** [insert k v m] mutates map [m] to bind [k] to [v]. If [k] was
      already bound in [m], that binding is replaced by the binding to
      [v]. *)
  val insert : 'k -> 'v -> ('k, 'v) t -> unit

  (** [find k m] is [Some v] if [m] binds [k] to [v], and [None] if [m]
      does not bind [k]. *)
  val find : 'k -> ('k, 'v) t -> 'v option

  (** [remove k m] mutates [m] to remove any binding of [k]. If [k] was
      not bound in [m], the map is unchanged. *)
  val remove : 'k -> ('k, 'v) t -> unit

  (** [create hash c] creates a new table map with capacity [c] that
      will use [hash] as the function to convert keys to integers.
      Requires: The output of [hash is always non-negative, and [hash]
      runs in the constant time]*)
  val create : ('k -> int) -> int -> ('k, 'v) t
  
  (** [bindings m] is an association list containing the same bindings
      as [m]. *)
  val bindings : ('k, 'v) t -> ('k * 'v) list

  (** [of_list hash lst] creates a map with the same bindings as [lst],
      using [hash] as the hash function. Requires: [lst] does not contain any duplicate keys. *)
  val of_list : ('k -> int) -> ('k * 'v) list -> ('k, 'v) t  
end

module LinearHashMap :TableMap = struct

  (** AF and RI: above *)
  type ('k, 'v) t = {
      hash : 'k -> int;
      mutable size : int;
      mutable buckets : ('k * 'v * int) option array
  }

  (** [capacity tab] is the number of buckets in [tab].
      Efficiency: O(1) *)
  let capacity (tbl : ('k, 'v) t) = Array.length tbl.buckets

  (** [load_factor tab] is the load factor of [tab], i.e., the number of
      bindings divided by the number of buckets. *)
  let load_factor tab =
    float_of_int (capacity tab) /. float_of_int tab.size

  (** Efficiency : O(n) *)
  let create hash n =
    { hash; size = 0; buckets = Array.init n (fun _  -> None)}


  let nextItem tab idx =
    if idx = capacity tab - 1 then
        0
    else
        idx + 1
                                

  
  let calIdx item = match item with
  | None -> true
  | Some (key, value, del) -> if del = 0 then true else false

  (** [index k tab] is the index at which key [k] should be stored in the
      buckets of [tab].
      Efficiency: O(1) *)
  let index k tab =
    let idx = (tab.hash k) mod (capacity tab) in
    let ln = capacity tab in
    let auxLst = List.init ln (fun a -> if idx + a <= ln - 1 then idx + a else idx + a - ln) in
    List.find (fun a -> let item = tab.buckets.(a) in calIdx item) auxLst

  (** [insert_no_resize k v tab] inserts a binding from [k] to [v] in [tab]
      and does not resize the table, regardless of what happens to the
      load factor.
      Efficiency: expected O(L) *)
  let insert_no_resize k v tab =
    let b = index k tab in
    tab.buckets.(b)  <- Some (k, v, 0);
    tab.size <- tab.size + 1

  let rehash tab new_capacity =
    let rehash_binding item = match item with
        | None -> ()
        | Some (k, v, d) -> if d = 0 then insert_no_resize k v tab else ()
    in

    let rehash_bucket bucket =
        Array.iter rehash_binding bucket
    in
    let old_buckets = tab.buckets in
    tab.buckets <- Array.init new_capacity (fun _ -> None);
    tab.size <- 0;
    rehash_bucket old_buckets

  let resize_if_needed tab =
    let lf = load_factor tab in
    if lf < 2.0 then
        rehash tab (capacity tab * 2)
    else if lf > 4.0 then
        rehash tab (capacity tab / 2)
    else ()

  let insert k v tab =
    resize_if_needed tab;
    insert_no_resize k v tab

  let find k tab =
    match tab.buckets.(index k tab) with
    | None -> None
    | Some (key, value ,del) -> if del = 0 then Some value else None

  let remove_no_resize k tab =
    let b = index k tab in
    match tab.buckets.(b) with
    | None -> ()
    | Some (k, v, d) -> tab.size <- tab.size - 1; tab.buckets.(b) <- Some (k, v, 1)

  let remove k tab =
    remove_no_resize k tab;
    resize_if_needed tab

  let bindings tab =
    Array.fold_left
      (fun acc bucket -> match bucket with
        | Some (k, v, d) -> if d = 0 then (k, v) :: acc else acc
        | None -> acc)
        [] tab.buckets

  let of_list hash lst =
    let m = create hash (List.length lst) in
    List.iter (fun (k, v) -> insert k v m) lst;
    m
end
(** An ['a node] is a node of mutable doubly-linked list.
    It contains a value of type ['a] and optionally has
    pointers to previous and/or next nodes. *)
type 'a node = {
  mutable prev : 'a node option;
  mutable next : 'a node option;
  value : 'a
}

(** An ['a dlist] is a mutable doubly-linked list with elements
    of type ['a]. It is possible to access the first and
    last elements in constant time.
    RI: The list does not contain any cycles. *)
type 'a dlist = {
  mutable first : 'a node option;
  mutable last : 'a node option;
}

let empty () : 'a dlist = { first = None; last = None }

let insert_first (lst : 'a dlist) (v : 'a) : unit =
  match lst.first with
    | Some fstnd -> ( match lst.last with
                        | Some lstnd ->(
                                          if fstnd == lstnd then (
                                            lst.first <- Some { prev = fstnd.prev; next = Some fstnd; value = v};
                                            lst.last <- Some fstnd;
                                            fstnd.prev <- lst.first
                                            )
                                          else (
                                            lst.first <- Some { prev = Some fstnd; next = fstnd.next; value = v};
                                            (
                                              match fstnd.next with
                                                | Some sndnd -> sndnd.prev <- Some fstnd
                                                | None -> failwith "Empty"
                                            )
                                            
                                          )
                                        )
                        | None -> failwith "Empty"
                    )
    | None -> (lst.first <- Some { prev = None; next = None; value = v}; lst.last <- lst.first)

(* let insert_last (lst : 'a dlist) (v : 'a) : unit =
  lst.last <- Some { prev = lst.last; next = lst.last; value = v} *)

let insert_last (lst : 'a dlist) (v : 'a) : unit =
  match lst.last with
    | Some lstnd -> ( match lst.first with
                        | Some fstnd -> if fstnd == lstnd then (
                                          lst.last <- Some { prev = Some lstnd; next = lstnd.next; value = v };
                                          fstnd.next <- lst.last
                                        )
                        | None -> failwith "Empty"
                    )
    | None -> (lst.first <- Some { prev = None; next = None; value = v}; lst.last <- lst.first)

let insert_after (n : 'a node option) (v : 'a) : unit = match n with
        | Some nd ->
              nd.next <- Some { prev = Some nd; next = nd.next; value = v}
        | None -> failwith "None"

let insert_before (n : 'a node option) (v : 'a) : unit = match n with
        | Some nd ->
              (match nd.prev with
                | None -> failwith "Empty"
                | Some prend -> prend.next <- Some {prev = Some prend; next = n; value = v}
              )
        | None -> failwith "Empty"
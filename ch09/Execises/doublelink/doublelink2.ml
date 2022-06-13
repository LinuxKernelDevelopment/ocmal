(** An ['a node] is a node of mutable doubly-linked list.
    It contains a value of type ['a] and optionally has
    pointers to previous and/or next nodes. *)
type 'a node = {
  mutable prev : 'a node option;
  mutable next : 'a node option;
  value : 'a
}

(** An ['a dlist] is mutable doubly-linked list with elements
    of type ['a]. It is possible to access the first and
    last elements in constant time.
    RI: The list does not contain any cycles. *)
type 'a dlist = {
  mutable first : 'a node option;
  mutable last : 'a node option;
}

let empty () : 'a dlist = { first = None; last = None }

let insert_before (ins : 'a node) (on : 'a node option)= 
  match on with
    | None -> ins.next <- None
    | Some nd -> ins.next <- on; nd.prev <- Some ins

let insert_after (ins : 'a node) (on : 'a node option) =
  match on with
    | None -> ins.prev <- None
    | Some nd -> nd.next <- Some ins;
                 ins.prev <- on

let remove_node (lst : 'a dlist) (ins : 'a node option) =
    let set_rel (lst : 'a dlist) (ins : 'a node) =
      match ins.prev, ins.next with
        | (None, None) -> lst.first <- None; lst.last <- None
        | (Some prev, Some next) -> prev.next <- Some next; next.prev <- Some prev
        | (Some prev, None) -> prev.next <- None; lst.last <- Some prev
        | (None, Some next) -> next.prev <- None; lst.first <- Some next
    in
      match ins with
       | Some nd -> set_rel lst nd
       | None -> ()

let insert_first (lst : 'a dlist) (v : 'a) : unit =
  match lst.first with
    | None -> lst.first <- Some { prev = None; next = None; value = v};
              lst.last <- lst.first
    | Some nd -> insert_before  {prev = None; next = lst.first; value = v} lst.first;
                 lst.first <- nd.prev
                 
let insert_last (lst : 'a dlist) (v : 'a) : unit =
  match lst.last with
    | None -> lst.last <- Some { prev = None; next = None; value = v};
              lst.first <- lst.last
    | Some nd -> insert_after {prev = lst.last; next = None; value = v} lst.last;
                 lst.last <- nd.next

let rec iter_list (lst : 'a dlist) (f : 'a -> 'a) =
  let retlst = empty () in
  let rec iter (ond : 'a node option) (func : 'a -> 'a) =
    match ond with
      | None -> ()
      | Some nd -> insert_last retlst (func nd.value); iter nd.next func
  in
    iter lst.first f; retlst

let rec riter_list (lst : 'a dlist) (f : 'a -> 'a) =
  let retlst = empty () in
  let rec riter (ond : 'a node option) (func : 'a -> 'a) =
    match ond with
      | None -> ()
      | Some nd -> insert_first retlst (func nd.value); riter nd.prev func
  in
    riter lst.last f; retlst
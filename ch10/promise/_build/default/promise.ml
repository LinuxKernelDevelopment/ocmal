(** A signature for Lwt-style promises, with better names *)
module type PROMISE = sig
  type 'a state =
    | Pending
    | Resolved of 'a
    | Rejected of exn

  type 'a promise

  type 'a resolver

  (** [make ()] is a new promise and resolver. The promise is pending. *)
  val make : unit -> 'a promise * 'a resolver

  (** [return x] is a new promise that is already resolved with value
      [x]. *)
  val return : 'a -> 'a promise

  (** [state p] is the state of promise *)
  val state : 'a promise -> 'a state

  (** [resolve r x] resolved the promise [p] associated with [r] with
      value [x], meaning that [state p] will become [Resolved x].
      Requires: [p] is pending. *)
  val resolve : 'a resolver -> 'a -> unit

  (** [reject r x] rejects the promise [p] associated with [r] with
      value [x], meaning that [state p] will become [Resolved x].
      Requires: [p] is pending. *)
  val reject : 'a resolver -> exn -> unit
end

module Promise = struct
  type 'a state =
    | Pending
    | Resolved of 'a
    | Rejected of exn

  type 'a promise = 'a state ref

  type 'a resolver = 'a promise

  (** [write_once p s] changes the state of [p] to be [s]. If [p] and
      [s] are both pending, that has no effect. Raises: [Invalid_arg] if
      the state of [p] is not pending. *)
  let write_once p s =
    if !p = Pending then p := s else invalid_arg "cannot write twice"

  let make () =
    let p = ref Pending in
    (p, p)

  let return x = ref (Resolved x)

  let state p = !p

  let resolve r x = write_once r (Resolved x)

  let reject r x = write_once r (Rejected x)
end
let next =
  let counter = ref 0 in
  fun () ->
    incr counter;
    !counter
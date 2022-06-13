(* AF: the float array [| x1; ...; xn |] represents the
 * vector (x1, ..., xn)
 * RI: the array is non-empty *)
type vector = float array

let norm (v : vector) =  Stdlib.sqrt (Array.fold_left (fun a b -> a +. b *. b) 0. v)

let normalize (v : vector) = let nr = norm v in Array.iteri (fun a b -> v.(a) <- v.(a) /. nr) v

let norm_loop (v : vector) = let sum = ref 0. in (for x = 1 to Array.length v  do sum := !sum +. v.(x - 1) done; Stdlib.sqrt !sum)

let normalize_loop (v : vector) = let nval = norm_loop v in for x = 1 to Array.length v do v.(x - 1) <- v.(x - 1) /. nval done
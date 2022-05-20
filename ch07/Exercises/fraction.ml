module type Fraction = sig

  type t

  val make : int -> int -> t

  val numerator : t -> int
  val denominator : t -> int
  val to_string : t -> string
  val to_float : t -> float

  val add : t -> t -> t
  val mul : t -> t -> t
end

module Fraction1 : Fraction = struct

   type t = int * int
   let rec gcd u v =
       if v <> 0 then (gcd v (u mod v))
       else u

   let make n d = (n, d)

   let numerator (n, d) = n
   let denominator (n, d) = d
   let to_string (n, d) = Printf.sprintf "%d / %d" n d
   let to_float (n, d) = (Float.of_int n) /. (Float.of_int d)

   let add (n1, d1) (n2, d2) =
           let nn1 = n1 * d2 + n2 * d1 in
           let nd1 = d1 * d2 in
              (nn1 / (gcd nn1 nd1), nd1 / (gcd nn1 nd1))

   let mul (n1, d1) (n2, d2) =
           let nn1 = n1 * n2 in
           let nd1 = d1 * d2 in
              (nn1 / (gcd nn1 nd1), nd1 / (gcd nn1 nd1))
end

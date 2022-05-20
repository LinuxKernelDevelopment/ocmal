(** [Poly] represents immutable polynomials with integer coefficients. *)
module type Poly = sig
  (** [t] is type of polynomials *)
  type t

  val empty : t

  val p1 : t

  val p2 : t

  (** [create x] creates polynomials based on x. *)
  val create : t -> t

  (** [combine p1 p2] combines [p1] and [p2]. Exmple: if [p1] represents
      $3x^2 + 2x$, [p2] represents $x^2 + 2x$, then [combine p1 p2] is
      $4x^2 + 4x$ *)
  val combine : t -> t -> t

  (** [eval x p] is [p] evaluated at [x]. Example: if [p] represents
      $3x^3 + x^2 + x$, then [eval 10 p] is [3110]. *)
  val eval : int -> t -> int

  val polyPrint_aux : int -> t -> string

  val polyPrint :
      Format.formatter -> t -> unit
end

let rec power v n = if n = 0 then 1 else v * power v (n - 1)

module ListPoly : Poly = struct
  type t = int list

  let empty = []

  let p1 = [1; 2; 3]

  let p2 = [3; 2; 1]

  let create (p : int list) = p

  let rec combine p1 p2 = match p1 with
        | ph1 :: pt1 -> (match p2 with
                          | ph2 :: pt2 -> [ph1 + ph2] @ combine pt1 pt2
                          | [] -> p1
                        )
        | [] -> p2

  let rec eval_aux v n poly = match poly with
         | h :: t -> (power v n) * h + eval_aux v (n + 1) t
         | [] -> 0

  let eval v poly = eval_aux v 0 poly

  let rec polyPrint_aux nr revPoly =
    match revPoly with
      | h :: t -> if t != [] then (Printf.sprintf "%dx^%d" h (nr - 1)) ^ " + " ^ (polyPrint_aux (nr - 1) t) else (Printf.sprintf "%dx^%d" h (nr - 1))
      | [] -> ""

  let polyPrint fmt poly = Format.fprintf fmt "%s" (polyPrint_aux  (List.length poly) (List.rev poly))
  (** let polyPrint fmt poly =
    let str = "" in
    let revLst = List.rev poly in
    let max = List.length revLst in
    for i = max - 1 to 1 do
      (if str = "" then
         str = Printf.sprintf "%dx^%d" (List.nth revLst (max - i)) i
      else
         str = Printf.sprintf "%s + %dx^%d" str (List.nth revLst (max - i)) i)
    done
    Format.fprintf fmt "%s" str*)
      

end

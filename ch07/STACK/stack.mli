module type S = sig
  type 'a t
  val empty : 'a t
  val push : 'a -> 'a t -> 'a t
end

module ListStack : S
module CustomStack : S

module type ToString = sig
  type t

  val to_string : t -> string

end

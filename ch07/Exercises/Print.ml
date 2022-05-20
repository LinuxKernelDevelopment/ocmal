module type ToString = sig
  type t
  val to_string : t -> string
end

module Print (M : ToString) = struct
  let print (arg : M.t) = M.to_string arg |> print_endline
end

module Print1 = functor (M : ToString) -> struct
  let print (arg : M.t) = M.to_string arg
end

module Int = struct
  type t = int
  let to_string arg = Printf.sprintf "%d" arg 
end

module MyString = struct
  type t = string
  let to_string arg = arg
end

module PrintInt = Print(Int)
module PrintString = Print(MyString)

module StringWithPrint = struct
  include String
  include PrintString
end

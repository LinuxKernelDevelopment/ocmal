module type X = sig
   val x : int
end

module IncX (M : X) = struct
   let x = M.x + 1
end

module AddX (M : X) = struct
   let add y = M.x + y
end

module type Add = sig val add : int -> int end

module CheckAddX : X -> Add = AddX

module type T = sig
  type t
  val x : t
end

module Pair1 (M : T) = struct
  let p = (M.x, 1)
end

module type P1 = functor (M : T) -> sig val p : M.t * int end

module Pair1 : P1 = functor (M : T) -> struct
  let p = (M.x, 1)
end

module P0 = Pair1 (struct type t = int let x = 0 end)
module PA = Pair1 (struct type t = char let x = 'a' end)

module F (M : sig val x : int end) = struct let y = M.x end
module X = struct let x = 0 end
module Z = struct let x = 0;; let z = 0 end
module FX = F (X)
module FZ = F (Z)

module type rbt = sig
  type color = Red | Black
  type 'a rbtree = Leaf | Node of color * 'a * 'a rbtree * 'a rbtree

  val mem : 'a -> 'a rbtree -> bool

  val balance : color * 'a * 'a rbtree * 'a rbtree -> 'a rbtree

  val insert : 'a -> 'a rbtree -> 'a rbtree
end

module Rbtree : rbt = struct
  type color = Red | Black
  type 'a rbtree = Leaf | Node of color * 'a * 'a rbtree * 'a rbtree
  let rec mem x = function
    | Leaf -> false
    | Node (_, y, l, r) ->
      if x < y then mem x l
      else if x > y then mem x r
      else true

  let balance = function
    | Black, z, Node (Red, y, Node (Red, x, a, b), c), d
    | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
    | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
    | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
      Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
    | a, b, c, d -> Node (a, b, c, d)

  let insert x s =
    let rec ins = function
      | Leaf -> Node (Red, x, Leaf, Leaf)
      | Node (color, y, a, b) as s ->
        if x < y then balance (color, y, ins a, b)
        else if x > y then balance (color, y, a, ins b)
        else s
      in
      match ins s with
      | Node (_, y, a, b) -> Node (Black, y, a, b)
      | Leaf -> failwith "RBT insert failed with ins returning leaf"
end

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module Make(Ord: OrderedType) =
  struct
    type elt = Ord.t
    type color = Red | Black
    type 'a rbtree = Leaf | Node of color * 'a * 'a rbtree * 'a rbtree

    type 'a t = Empty | Root of 'a rbtree
    let rec mem x = function
    | Leaf -> false
    | Node (_, y, l, r) ->
      if Ord.compare x y < 0 then mem x l
      else if Ord.compare x y > 0 then mem x r
      else true

  let balance = function
    | Black, z, Node (Red, y, Node (Red, x, a, b), c), d
    | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
    | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
    | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
      Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
    | a, b, c, d -> Node (a, b, c, d)

  let insert x s =
    let rec ins = function
      | Leaf -> Node (Red, x, Leaf, Leaf)
      | Node (color, y, a, b) as s ->
        if Ord.compare x y < 0 then balance (color, y, ins a, b)
        else if Ord.compare x y > 0 then balance (color, y, a, ins b)
        else s
      in
      match ins s with
      | Node (_, y, a, b) -> Node (Black, y, a, b)
      | Leaf -> failwith "RBT insert failed with ins returning leaf"
  end
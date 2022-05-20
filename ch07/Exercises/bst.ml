module type Map =
 sig
  type ('k, 'v) t

  exception NotFound

  val empty : ('k, 'v) t

  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

  val lookup : 'k -> ('k, 'v) t -> 'v

  val bindings : ('k, 'v) t -> ('k * 'v) list
 end


type 'a tree =
  | Leaf
  | Node of 'a node
and 'a node = {
  value: 'a;
  left: 'a tree;
  right: 'a tree;
}

module BstMap : Map = struct
  

  type ('k, 'v) t = ('k * 'v) tree

  exception NotFound

  let empty = Leaf

  let rec insert k v m = match m with
       | Leaf -> let value = (k, v) in 
                Node {value=value; left=Leaf; right=Leaf;}
       | Node {value; left; right} -> let (mk, mv) = value in
                                      if k = mk then Node{value=value; left=left; right=right}
                                      else if k < mk then Node{value=value; left=insert k v left; right=right}
                                      else Node{value=value; left=left; right=insert k v right}
                            

  let rec lookup k m = match m with
       | Leaf -> raise NotFound
       | Node {value; left; right} -> let (mk, mv) = value in
                                      if k = mk then mv else if k < mk then lookup k left else lookup k right
                           

  let rec bindings = function
       | Leaf -> []
       | Node {value; left; right} -> [value] @ bindings left @ bindings right
end  

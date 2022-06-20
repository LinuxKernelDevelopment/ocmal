type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec preorder = function
  | Leaf -> []
  | Node (l, v, r) -> [v] @ preorder l @ preorder r

let rec inorder = function
  | Leaf -> []
  | Node (l, v, r) -> inorder l @ [v] @ inorder r

let rec postorder = function
  | Leaf -> []
  | Node (l, v, r) -> postorder l @ postorder r @ [v]

let t =
  Node(Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)),
       4,
       Node(Node(Leaf, 5, Leaf), 6, Node(Leaf, 7, Leaf)))

let () = assert (preorder t =  [4;2;1;3;6;5;7])
let () = assert (inorder t  =  [1;2;3;4;5;6;7])
let () = assert (postorder t = [1;3;2;5;7;6;4])
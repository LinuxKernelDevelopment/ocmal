type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec preorder_aux (lst : int list ref) = function
  | Leaf -> ()
  | Node (l, v, r) -> lst := v :: !lst; preorder_aux lst l; preorder_aux lst r

let preorder r =
  let lst = ref [] in
    preorder_aux lst r;
    List.rev !lst

let rec inorder_aux (lst : int list ref) = function
  | Leaf -> ()
  | Node (l, v, r) -> inorder_aux lst  l; lst := v :: !lst; inorder_aux lst r

let inorder t =
  let tmp = ref [] in
    inorder_aux tmp t; List.rev !tmp

let rec postorder_aux (lst : int list ref) node = match node with
  | Leaf -> ()
  | Node (l, v, r) -> postorder_aux lst l;  postorder_aux lst r; lst := v :: !lst

let postorder t =
  let tmp = ref [] in
    postorder_aux tmp t; List.rev !tmp


let t =
  Node(Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)),
       4,
       Node(Node(Leaf, 5, Leaf), 6, Node(Leaf, 7, Leaf)))

let () = assert (preorder t =  [4;2;1;3;6;5;7])
let () = assert (inorder t  =  [1;2;3;4;5;6;7])
let () = assert (postorder t = [1;3;2;5;7;6;4])
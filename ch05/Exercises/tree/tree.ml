type 'a tree =
      | Leaf
      | Node of 'a node
and 'a node = {
     value: 'a;
     left: 'a tree;
     right: 'a tree;
}

let t =
  Node {
   value = 2;
   left = Node {value = 1; left = 
           Node {value = 4; left = Leaf; right = Leaf}; 
           right = Leaf};
   right = Node {value = 3; left = Leaf; right = Leaf}
  }

let height t =
        let rec heightr t n  = match t with
            | Leaf -> n
            | Node {value; left; right} -> max (heightr left (n + 1)) (heightr right (n + 1))
         in
            heightr t 0

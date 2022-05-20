type 'a tree =
      | Leaf
      | Node of 'a node
and 'a node = {
     value: 'a;
     left: 'a tree;
     right: 'a tree;
}

let t1 =
  Node {
   value = 2;
   left = Node {value = 1; left = 
           Node {value = 4; left = Leaf; right = Leaf}; 
           right = Leaf};
   right = Node {value = 3; left = Leaf; right = Leaf}
  }

let t2 =
  Node {
   value = 2;
   left = Node {value = 1; left = 
           Node {value = 4; left = Leaf; right = Leaf}; 
           right = 
           Node {value = 5; left = Leaf; right = Leaf}
           };
   right = Node {value = 3; left = Leaf; right = Leaf}
  }

let height t =
        let rec heightr t n  = match t with
            | Leaf -> n
            | Node {value; left; right} -> max (heightr left (n + 1)) (heightr right (n + 1))
         in
            heightr t 0


let shape t1 t2 =
        let rec shape_help t1 t2 = match t1 with
                         | Leaf -> begin match t2 with
                                         | Leaf -> 0
                                         | Node {value; left; right} -> 1
                                   end
                         | Node {value = v1; left = l1; right = r1} -> begin match t2 with
                                                              | Leaf -> 1
                                                              | Node {value = v2; left = l2; right = r2} -> shape_help l1 l2 + shape_help r1 r2
                                                end in
        shape_help t1 t2 = 0

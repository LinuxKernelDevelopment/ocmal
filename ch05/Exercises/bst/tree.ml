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
   left = Node {value = 1; left = Leaf;
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

let is_bst t =
        let rec is_bst_help = function
                | Leaf -> true
                | Node {value = root_value; left = root_left; right = root_right} -> begin match root_left with
                                                                                           | Leaf -> begin match root_right with
                                                                                                           | Leaf -> true
                                                                                                           | Node {value = right_value; left = right_left; right = right_right} -> root_value <= right_value && is_bst_help right_left && is_bst_help right_right
                                                                                                     end
                                                                                           | Node {value = left_value; left = left_left; right = left_right} -> begin match root_right with
                                                                                                                                                                      | Leaf -> left_value <= root_value && is_bst_help left_left && is_bst_help left_right
                                                                                                                                                                      | Node {value = right_value; left = right_left; right = right_right} ->
                                                                                                                                                                                left_value <= root_value && root_value <= right_value && 
                                                                                                                                                                                is_bst_help left_left && is_bst_help left_right &&
                                                                                                                                                                                is_bst_help right_left && is_bst_help right_right
                                                                                                                                                                end
                                                                                     end in
        is_bst_help t

let is_bst2 t =
        let check_left = function
                | Leaf -> true
                | Node {value = vRoot; left = lRoot; right = rRoot} -> (
                          match lRoot with 
                              | Leaf -> true
                              | Node {value = vLeft; left = lLeft; right = rLeft} -> vLeft < vRoot
                        ) in
        let check_right = function
                | Leaf -> true
                | Node {value = vRoot; left = lRoot; right = rRoot} -> (
                        match rRoot with
                              | Leaf -> true
                              | Node {value = vRight; left = lRight; right = _} -> vRoot < vRight
                        ) in
       let rec is_bst_help t = match t with
               | Leaf -> true
               | Node {value; left; right} -> check_left t && check_right t && is_bst_help left && is_bst_help right
       in
           is_bst_help t

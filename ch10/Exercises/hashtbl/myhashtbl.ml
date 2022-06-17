let func =
  let tbl = Hashtbl.create 16 in
  for elem = 1 to 31 do
    Hashtbl.add tbl elem (string_of_int elem)
  done;
  tbl

let bindings tbl =
  let lst = [] in
  Hashtbl.fold (fun a b lst -> (a, b) :: lst) tbl lst

let load_factor tbl  =
  let status = Hashtbl.stats tbl in 
  float_of_int status.num_bindings /.  float_of_int status.num_buckets

module StringHash =
  struct
    type t = string
    let equal i j = i = j
    let hash i =
      let hashval = ref 0 in
      (for idx = 0 to (String.length i) - 1 do
        ignore (hashval := !hashval + (int_of_char i.[idx]))
      done; Printf.printf "%d\n" !hashval; !hashval)
  end

module StringHashtbl = Hashtbl.Make(StringHash)
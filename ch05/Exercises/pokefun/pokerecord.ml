type poketype = Normal | Fire | Water

type pokemon = { name : string; hp : int; ptype : poketype }

let charizard = { name = "Charizard"; hp = 78; ptype = Fire }

let squirtle = { name = "Squirtle"; hp = 44; ptype = Water }

let clefairy = { name = "Clefairy"; hp = 387; ptype = Normal }

let pikachu = { name = "Pikachu"; hp = 126; ptype = Normal}

let ninetales = { name = "Ninetales"; hp = 621; ptype = Normal }

let pokemonlst = [ charizard; squirtle; clefairy; pikachu; ninetales ]

let rec max_hp lst acc = match lst with
        | h :: t -> if h.hp > acc.hp then 
                            max_hp t h
                    else 
                            max_hp t acc
        | [] -> acc

let tmp = { name = "tmp"; hp = 0; ptype = Normal }

let ans = max_hp pokemonlst tmp

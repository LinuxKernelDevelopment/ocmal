type ptype = TNormal | TFire | Twater

type mon = { name : string; hp : int; ptype : ptype }

let get_hp m = match m with { name = n; hp = h; ptype = t } -> h

let get_hp m = match m with { name = _; hp = h; ptype = _ } -> h

let get_hp m = match m with { name; hp; ptype } -> hp

let get_hp m = match m with { hp } -> hp

let get_hp m = m.hp

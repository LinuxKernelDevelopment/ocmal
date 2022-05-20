let add x y = x + y

let add'' (x, y) = x + y

let curry f x y = f (x, y)
let uncurry f (x, y) = f x y

let uncurried_add = uncurry add
let curried_add = curry add''

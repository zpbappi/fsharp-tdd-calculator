module internal RpnBinaryOperations

let inline add x y = x + y
let inline subtract x y = x - y
let inline multiple x y = x * y

let divide x y : float =
    match y with
    | 0. -> raise (invalidOp "Cannot divide by zero")
    | _ -> x/y

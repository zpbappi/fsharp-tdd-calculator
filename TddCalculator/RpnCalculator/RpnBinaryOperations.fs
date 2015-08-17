module RpnBinaryOperations

let add x y : float = x + y
let subtract x y : float = x - y
let multiply x y : float = x * y

let divide x y : float =
    match y with
    | 0. -> raise (invalidOp "Cannot divide by zero")
    | _ -> x/y

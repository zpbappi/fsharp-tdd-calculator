module RpnBinaryOperations

let add x y : decimal = x + y
let subtract x y : decimal = x - y
let multiply x y : decimal = x * y

let divide x y : decimal =
    match y with
    | 0m -> raise (invalidOp "Cannot divide by zero")
    | _ -> x/y

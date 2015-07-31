module Stack

type Stack<'a> = 'a list

let isEmpty (stack : Stack<'a>) =
    match stack with 
    | [] -> true
    | _ -> false

let push item stack : Stack<'a> =
    item :: stack

let pop (stack : Stack<'a>) =
    match stack with
    | [] -> raise (invalidOp "Stack underflow")
    | top::rest -> (top, rest)

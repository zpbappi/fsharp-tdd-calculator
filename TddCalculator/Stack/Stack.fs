module Stack

type Stack<'a> = 'a list

let isEmpty (stack : Stack<'a>) =
    match stack with 
    | [] -> true
    | _ -> false

let push (item : 'a) (stack : Stack<'a>) : Stack<'a> =
    item :: stack

let pop (stack : Stack<'a>) : 'a * Stack<'a> =
    (stack.Head, stack.Tail)
module Stack

type Stack<'a> = 'a list

let isEmpty (stack : Stack<'a>) =
    match stack with 
    | [] -> true
    | _ -> false

let push (item : 'a) (stack : Stack<'a>) =
    []

let pop (stack : Stack<'a>) =
    (0, [])
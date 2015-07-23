module Stack

type Stack<'a> = 'a list

let isEmpty (stack : Stack<'a>) =
    match stack with 
    | [] -> true
    | _ -> false
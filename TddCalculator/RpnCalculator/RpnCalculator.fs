module RpnCalculator

open Stack

let calculate (stack : Stack<string>) =
    match stack with
    | [a; b; "+"] -> System.Int32.Parse(a) + System.Int32.Parse(b)
    | _ -> 0


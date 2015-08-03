module RpnCalculator

open Stack
open RpnTypes

module Utility =
    open RpnTypes

    let private (|IsInt|_|) str =
        match System.Int32.TryParse(str) with
        | (success, number) when success -> Some number
        | _ -> None

    let private (|IsFloat|_|) str =
        match System.Single.TryParse(str) with
        | (success, number) when success -> Some (float number)
        | _ -> None

    let parse str=
        match str with 
        | null | "" -> None
        | IsInt x -> Some (Operand (Integer x))
        | IsFloat f -> Some (Operand (Float f))
        | _ -> Some (Operator str)

    let internal isAWholeNumber (f : float) = System.Math.Round f = f;


let calculate (stack : Stack<string>) =
    match stack with
    | [a; b; "+"] -> System.Int32.Parse(a) + System.Int32.Parse(b)
    | [a; b; "-"] -> System.Int32.Parse(a) - System.Int32.Parse(b)
    | _ -> 0


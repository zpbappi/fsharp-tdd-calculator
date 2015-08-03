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


let private getOperation operator =
    match operator with
    | "+" -> RpnBinaryOperations.add
    | "-" -> RpnBinaryOperations.subtract
    | "*" -> RpnBinaryOperations.multiple
    | "/" -> RpnBinaryOperations.divide
    | _ -> raise (invalidOp (sprintf "Unknown operator: %s" operator))

let private applyBinaryOperands x y operation =
    let act =
        match (x, y) with
        | Integer a, Integer b -> operation (float a) (float b)
        | Integer a, Float b -> operation (float a) b
        | Float a, Integer b -> operation a (float b)
        | Float a, Float b -> operation a b

    let convert res =
        if Utility.isAWholeNumber res then Integer (int res)
        else Float res

    act |> convert

let evaluateRpnExpr (stack : Stack<string>) =
    let convertNumber str =
        match Option.get(Utility.parse str) with
        | Operand n -> n
        | _ -> raise (System.Exception "should not happen")
    
    match stack with 
    | [a; b; op] ->
        let x, y = convertNumber a, convertNumber b
        op |> getOperation |> applyBinaryOperands x y
    | _ -> Integer 0


let calculate (stack : Stack<string>) =
    match evaluateRpnExpr stack with
    | Integer n -> n
    | Float f -> int f


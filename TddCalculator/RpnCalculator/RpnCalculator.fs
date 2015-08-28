module RpnCalculator

open Stack
open RpnTypes

module Utility =
    open RpnTypes

    let private (|IsInt|_|) str =
        match System.Int32.TryParse(str) with
        | (success, number) when success -> Some number
        | _ -> None

    let private (|IsDecimal|_|) str =
        match System.Decimal.TryParse(str) with
        | (success, number) when success -> Some number
        | _ -> None

    let parse str =
        match str with 
        | null | "" -> None
        | IsInt x -> Some (Operand (Integer x))
        | IsDecimal d -> Some (Operand (Decimal d))
        | _ -> Some (Operator str)

    let internal isAWholeNumber (d : decimal) = System.Math.Round d = d;


let private getOperation operator =
    match operator with
    | "+" -> RpnBinaryOperations.add
    | "-" -> RpnBinaryOperations.subtract
    | "*" -> RpnBinaryOperations.multiply
    | "/" -> RpnBinaryOperations.divide
    | _ -> raise (invalidOp (sprintf "Unknown operator: %s" operator))

let private applyBinaryOperands x y operation =
    let act =
        match (x, y) with
        | Integer a, Integer b -> operation (decimal a) (decimal b)
        | Integer a, Decimal b -> operation (decimal a) b
        | Decimal a, Integer b -> operation a (decimal b)
        | Decimal a, Decimal b -> operation a b

    let convert res =
        if Utility.isAWholeNumber res then Integer (int res)
        else Decimal res

    act |> convert

let private evaluateRpnExpr state item =
    match item with
    | Operand x -> Stack.push x state
    | Operator op ->
        let (y, s1) = Stack.pop state
        let (x, s2) = Stack.pop s1
        let res = op 
                |> getOperation
                |> applyBinaryOperands x y
        Stack.push res s2

type RpnResult = 
    | DecimalResult of decimal
    | IntegerResult of int
    | Error of string

let calculate (stack : Stack<string>) =
    let result = stack 
                |> List.map Utility.parse 
                |> List.filter Option.isSome 
                |> List.map Option.get
                |> List.fold evaluateRpnExpr []

    let resultConverter res =
        match res with 
        | Integer n -> IntegerResult n
        | Decimal d -> DecimalResult d

    match result with
    | [] -> IntegerResult 0
    | [n] -> resultConverter n
    | _ -> Error "Why! why!! why!!!"


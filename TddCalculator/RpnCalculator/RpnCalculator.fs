namespace RpnCalculator

open Stack
open RpnTypes

module Utility =
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



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RpnCalculator =
    let private getOperation (calculator : RpnCalculator) operator =
        
        let operatorMatch operation =
            match operation with
            | BinaryOperation(op, _) -> op = operator
            
        match List.tryFind operatorMatch calculator.operations with
        | Some (BinaryOperation (_, operation)) -> operation
        | None -> raise (invalidOp (sprintf "Unknown operator: %s" operator))
        

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

    let private evaluateRpnExpr (calculator, stack) item =
        match item with
        | Operand x -> (calculator, Stack.push x stack)
        | Operator op ->
            let (y, s1) = Stack.pop stack
            let (x, s2) = Stack.pop s1
            let res = op 
                    |> getOperation calculator
                    |> applyBinaryOperands x y
            (calculator, Stack.push res s2)


    let createInstance ()= 
        {
            operations = 
            [
                BinaryOperation ("+", RpnBinaryOperations.add)
                BinaryOperation ("-", RpnBinaryOperations.subtract)
                BinaryOperation ("*", RpnBinaryOperations.multiply)
                BinaryOperation ("/", RpnBinaryOperations.divide)
            ]
        }

    let registerBinaryOperation operator operation calculator =
        { operations = BinaryOperation(operator, operation) :: calculator.operations }

    let calculate (stack : Stack<string>) calculator =
        let (_, result) = stack 
                        |> List.map Utility.parse 
                        |> List.filter Option.isSome 
                        |> List.map Option.get
                        |> List.fold evaluateRpnExpr (calculator, [])

        let resultConverter res =
            match res with 
            | Integer n -> IntegerResult n
            | Decimal d -> DecimalResult d

        match result with
        | [] -> IntegerResult 0
        | [n] -> resultConverter n
        | _ -> Error "Why! why!! why!!!"


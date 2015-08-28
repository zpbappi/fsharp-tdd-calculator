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
            | UnaryOperation(op, _) -> op = operator
            
        match List.tryFind operatorMatch calculator.operations with
        | Some op -> op
        | None -> raise (invalidOp (sprintf "Unknown operator: %s" operator))
        
    let private operandConversion number =
        if Utility.isAWholeNumber number then Integer (int number)
            else Decimal number

    let private applyBinaryOperands x y operation =
        match (x, y) with
        | Integer a, Integer b -> operation (decimal a) (decimal b)
        | Integer a, Decimal b -> operation (decimal a) b
        | Decimal a, Integer b -> operation a (decimal b)
        | Decimal a, Decimal b -> operation a b

    let private applyUnaryOperands x operation =
        match x with
        | Integer a -> operation (decimal a)
        | Decimal a -> operation a

    let private applyOperation stack operation =
        let (st, res) = match operation with
                            | BinaryOperation (_, op) -> 
                                let (y, s1) = Stack.pop stack
                                let (x, s2) = Stack.pop s1
                                (s2, applyBinaryOperands x y op)
                            | UnaryOperation (_, op) ->
                                let (x, s1) = Stack.pop stack
                                (s1, applyUnaryOperands x op)

        Stack.push (res |> operandConversion) st

    let private evaluateRpnExpr (calculator, stack) item =
        match item with
        | Operand x -> (calculator, Stack.push x stack)
        | Operator op ->
            let resStack = op 
                        |> getOperation calculator
                        |> applyOperation stack
            (calculator, resStack)


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

    let registerUnaryOperation operator operation calculator =
        { operations = UnaryOperation(operator, operation) :: calculator.operations }

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


namespace RpnCalculator

open Stack

module RpnTypes =
    type Number =
        | Integer of int
        | Decimal of decimal

    type RpnItem = 
        | Operand of Number
        | Operator of string

    type Operation =
        | BinaryOperation of (string * Number -> Number -> Number)


type RpnCalculator = {
    stack : Stack<RpnTypes.RpnItem>
    operations : RpnTypes.Operation list
}

type RpnResult = 
    | DecimalResult of decimal
    | IntegerResult of int
    | Error of string
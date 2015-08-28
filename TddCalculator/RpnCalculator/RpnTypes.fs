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
        | BinaryOperation of string * (decimal -> decimal -> decimal)


type RpnCalculator = {
    operations : RpnTypes.Operation list
}

type RpnResult = 
    | DecimalResult of decimal
    | IntegerResult of int
    | Error of string
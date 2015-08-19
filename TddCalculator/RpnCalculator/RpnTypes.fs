module RpnTypes

open Stack

type Number =
    | Integer of int
    | Decimal of decimal

type RpnItem = 
    | Operand of Number
    | Operator of string

type Operation =
    | BinaryOperation of (string * Number -> Number -> Number)

type RpnCalculator = {
    stack : Stack<RpnItem>
    operations : Operation list
}

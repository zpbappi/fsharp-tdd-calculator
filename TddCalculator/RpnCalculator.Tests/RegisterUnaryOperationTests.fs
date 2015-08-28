module RpnCalculatorTests.RegisterUnaryOperation

open Xunit
open Swensen.Unquote
open RpnCalculator

let sqrt (number : decimal) = decimal (System.Math.Sqrt(float(number)))
let sqr (number : decimal) = number * number
let ln (number : decimal) = decimal(System.Math.Log(float(number)))
let e (number : decimal) = decimal(System.Math.Exp(float(number)))

let getCalculator () =
    RpnCalculator.createInstance()
    |> RpnCalculator.registerUnaryOperation "sqrt" sqrt
    |> RpnCalculator.registerUnaryOperation "sqr" sqr
    |> RpnCalculator.registerUnaryOperation "ln" ln
    |> RpnCalculator.registerUnaryOperation "e" e

[<Fact>]
let ``should be able to register unary operator`` () =
    let calc = getCalculator()
    test <@ RpnCalculator.calculate ["3"; "sqr"] calc = IntegerResult 9 @>

[<Fact>]
let ``sqrt of a sqr should be the same`` () =
    let calc = getCalculator()
    test <@ RpnCalculator.calculate ["42"; "sqr"; "sqrt"] calc = IntegerResult 42 @>
    test <@ RpnCalculator.calculate ["3.14"; "sqr"; "sqrt"] calc = DecimalResult 3.14m @>

[<Fact>]
let ``ln of e^x should be x`` () =
    let calc = getCalculator()
    test <@ RpnCalculator.calculate ["42"; "e"; "ln"] calc = IntegerResult 42 @>
    test <@ RpnCalculator.calculate ["3.14"; "e"; "ln"] calc = DecimalResult 3.14m @>

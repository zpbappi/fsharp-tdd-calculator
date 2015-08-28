module RpnCalculatorTests.RegisterBinaryOperation

open Xunit
open Swensen.Unquote

open RpnCalculator

let getCalculatorWithPow () =
    RpnCalculator.createInstance() 
    |> RpnCalculator.registerBinaryOperation "^" (fun (a : decimal) (b : decimal)-> decimal(System.Math.Pow(float(a), float(b))))

[<Fact>]
let ``should be able to register operation externally`` () =
    let calc = getCalculatorWithPow();

    test <@ RpnCalculator.calculate ["3"; "2"; "^"] calc = IntegerResult 9 @>

[<Fact>]
let ``should be able to perform newly registered operation with previous ones`` () =
    let calc = getCalculatorWithPow()

    test <@ RpnCalculator.calculate ["32.1404"; "3.14"; "2"; "^"; "+"] calc = IntegerResult 42 @>
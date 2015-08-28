module RpnCalculatorTests.Trivial

open Xunit
open Swensen.Unquote

open RpnCalculator

[<Fact>]
let ``calculation on empty stack results in zero`` () =
    test <@ RpnCalculator.calculate [] = IntegerResult 0 @>

[<Fact>]
let ``a single integer will always return the same number`` () =
    test <@ RpnCalculator.calculate ["42"] = IntegerResult 42 @>

[<Fact>]
let ``a single decimal will always return the same number`` () =
    test <@ RpnCalculator.calculate ["3.14"] = DecimalResult 3.14m @>
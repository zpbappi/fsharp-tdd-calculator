module RpnCalculatorTests.Trivial

open Xunit
open Swensen.Unquote

open RpnCalculator

[<Fact>]
let ``calculation on empty stack results in zero`` () =
    let calc = RpnCalculator.createInstance()
    test <@ RpnCalculator.calculate [] calc = IntegerResult 0 @>

[<Fact>]
let ``a single integer will always return the same number`` () =
    let calc = RpnCalculator.createInstance()
    test <@ RpnCalculator.calculate ["42"] calc = IntegerResult 42 @>

[<Fact>]
let ``a single decimal will always return the same number`` () =
    let calc = RpnCalculator.createInstance()
    test <@ RpnCalculator.calculate ["3.14"] calc = DecimalResult 3.14m @>
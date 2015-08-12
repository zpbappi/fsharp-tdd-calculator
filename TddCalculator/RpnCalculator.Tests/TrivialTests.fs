module RpnCalculatorTests.Trivial

open Xunit
open Swensen.Unquote

open RpnCalculator

[<Fact>]
let ``calculation on empty stack results in zero`` () =
    test <@ calculate [] = Integer 0 @>
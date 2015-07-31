module RpnCalculatorTests

open Xunit
open Swensen.Unquote

open RpnCalculator

[<Fact>]
let ``calculation on empty stack results in zero`` () =
    test <@ calculate [] = 0 @>

[<Fact>]
let ``should be able to add two numbers from stack`` () =
    test <@ calculate ["1"; "1"; "+"] = 2 @>

[<Theory>]
[<InlineData("1", "1", 2)>]
[<InlineData("0", "0", 0)>]
[<InlineData("1", "2", 3)>]
[<InlineData("40", "2", 42)>]
let ``should be able to add any arbitrary two integers`` (x : string) (y : string) (res: int) =
    test <@ calculate [x; y; "+"] = res @>
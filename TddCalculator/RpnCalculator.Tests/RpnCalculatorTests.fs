﻿module RpnCalculatorTests

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

[<Fact>]
let ``should be able to subtract two numbers`` () =
    test <@ calculate ["2"; "1"; "-"] = 1 @>

[<Theory>]
[<InlineData("1", "1", 0)>]
[<InlineData("5", "3", 2)>]
[<InlineData("42", "2", 40)>]
[<InlineData("6", "9", -3)>]
let ``should be able to stract two arbitrary integers`` (x : string) (y : string) (res : int) =
    test <@ calculate [x; y; "-"] = res @>
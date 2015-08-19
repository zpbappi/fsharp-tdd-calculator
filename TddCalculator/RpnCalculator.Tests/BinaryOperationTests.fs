module RpnCalculatorTests.BinaryOperations

open Xunit
open Swensen.Unquote

open RpnCalculator

[<Fact>]
let ``should be able to add two numbers from stack`` () =
    test <@ calculate ["1"; "1"; "+"] = Integer 2 @>

[<Theory>]
[<InlineData("1", "1", 2)>]
[<InlineData("0", "0", 0)>]
[<InlineData("1", "2", 3)>]
[<InlineData("40", "2", 42)>]
let ``should be able to add any arbitrary two integers`` (x : string) (y : string) (res: int) =
    test <@ calculate [x; y; "+"] = Integer res @>

[<Fact>]
let ``should be able to subtract two numbers`` () =
    test <@ calculate ["2"; "1"; "-"] = Integer 1 @>

[<Theory>]
[<InlineData("1", "1", 0)>]
[<InlineData("5", "3", 2)>]
[<InlineData("42", "2", 40)>]
[<InlineData("6", "9", -3)>]
let ``should be able to stract two arbitrary integers`` (x : string) (y : string) (res : int) =
    test <@ calculate [x; y; "-"] = Integer res @>


[<Theory>]
[<InlineData("1", "2", "+", "3")>]
[<InlineData("1.2", "2.3", "+", "3.5")>]
[<InlineData("43.9", "1.9", "-", "42")>]
[<InlineData("2", "1.14", "+", "3.14")>]
[<InlineData("42", "0.0", "+", "42")>]
[<InlineData("41", "1.0", "+", "42")>]
[<InlineData("2.6", "3.7", "+", "6.3")>]
let ``should be able to handle int and float operands`` (x : string) (y : string) (operator : string) (resVal : string) =
    let (|IsInt|_|) x = match System.Int32.TryParse(x) with | (s, v) when s -> Some v | _ -> None
    let (|IsFloat|_|) x = match System.Double.TryParse(x) with | (s, v) when s -> Some v | _ -> None
    let res = match resVal with | IsInt n-> Integer n | IsFloat f -> Float f | _ -> raise(System.Exception("Unknown"))

    test <@ calculate [x; y; operator] = res @>
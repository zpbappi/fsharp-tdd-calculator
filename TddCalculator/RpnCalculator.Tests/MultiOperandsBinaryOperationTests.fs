module RpnCalculatorTests.MultiOperandsBinaryOperation

open Xunit
open Swensen.Unquote

open RpnCalculator

[<Theory>]
[<InlineData("1, 1, +", "2")>]
[<InlineData("1, 1, -", "0")>]
[<InlineData("1, 1, *", "1")>]
[<InlineData("1, 1, /", "1")>]
[<InlineData("40, 1, 1, +, +", "42")>]
[<InlineData("5, 1, -, 3, 1, +, +", "8")>]
[<InlineData("5, 6, 2, 5, *, -, +", "1")>]
let ``should produce proper result for multiple operators`` (stackedString : string) (resVal : string) =
    let (|IsInt|_|) x = match System.Int32.TryParse(x) with | (s, v) when s -> Some v | _ -> None
    let (|IsDecimal|_|) x = match System.Decimal.TryParse(x) with | (s, v) when s -> Some v | _ -> None
    let res = match resVal with | IsInt n-> IntegerResult n | IsDecimal d -> DecimalResult d | _ -> raise(System.Exception("Should not happen"))
    
    let trim (str : string) = str.Trim()
    let stack = stackedString.Split(',') |> Array.toList |> List.map trim

    test <@ calculate stack = res @>

module RpnCalculatorTests.ParseString

open Xunit
open Swensen.Unquote
open RpnTypes
open RpnCalculator

[<Fact>]
let ``should return nothing for empty or null strings`` () =
    test <@ Utility.parse null = None @>
    test <@ Utility.parse "" = None @>

[<Theory>]
[<InlineData("0", 0)>]
[<InlineData("1", 1)>]
[<InlineData("42", 42)>]
[<InlineData("-1", -1)>]
[<InlineData("-390", -390)>]
let ``should parse interger numbers correctly`` (str : string) (num : int) =
    test <@ Utility.parse str = Some (Operand (Number.Integer num)) @>

[<Theory>]
[<InlineData("1.2", 1.2f)>]
[<InlineData("3.1415", 3.1415f)>]
[<InlineData("-4.2", -4.2f)>]
[<InlineData("-1.", -1.f)>]
let ``should parse float numbers correctly`` (str : string) (num : float) =
    test <@ Utility.parse str = Some (Operand (Number.Float num)) @>

[<Theory>]
[<InlineData("+")>]
[<InlineData("-")>]
[<InlineData("++")>]
[<InlineData("may-be-operator")>]
let ``should parse operators correctly`` (op : string) =
    test <@ Utility.parse op = Some (Operator op) @>
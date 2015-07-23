module Stack.Tests

open Xunit
open Swensen.Unquote
open Stack

[<Fact>]
let ``Stack should be empty for empty list`` () =
    test <@ isEmpty [] @>

[<Fact>]
let ``Stack should not be empty for non-empty list`` () =
    ()

[<Fact>]
let ``empty checking should work for stack of any arbitrary type`` () =
    ()
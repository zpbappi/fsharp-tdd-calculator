module Stack.Tests

open Xunit
open Swensen.Unquote
open Stack

[<Fact>]
let ``Stack should be empty for empty list`` () =
    test <@ isEmpty [] @>

[<Fact>]
let ``Stack should not be empty for non-empty list`` () =
    test <@ isEmpty [1; 2; 3] = false @>


type Student = {
    Name : string
    Roll : int
    CGPA : float
}
[<Fact>]
let ``empty checking should work for stack of any arbitrary type`` () =
    let intStack = [1; 2; 3]
    let strStack = ["a"; "b"]
    let floatStack = [1.; 3.9]
    let studentStack = [
        {Name = "Mr. A"; Roll = 1; CGPA = 4.0};
        {Name = "Mr. B"; Roll = 2; CGPA = 3.95}
    ]

    test <@ isEmpty intStack = false @>
    test <@ isEmpty strStack = false @>
    test <@ isEmpty floatStack = false @>
    test <@ isEmpty studentStack = false @>

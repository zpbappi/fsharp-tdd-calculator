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


[<Fact>]
let ``pushing to empty stack return stack with single element`` () =
    test <@ push 1 [] = [1] @>

[<Fact>]
let ``pushing to any non-empty stack places the item at first`` () =
    test <@ push 1 [2; 3] = [1; 2; 3] @>

[<Fact>]
let ``popping from single element stack returns the item and empty stack`` () =
    test <@ pop [1] = (1, []) @>

[<Fact>]
let ``popping from arbitrary stack return the first item and rest of the stack`` () =
    test <@ pop [1; 2; 3] = (1, [2; 3]) @>

[<Fact>]
let ``popping from empty stack throws stack underflow exception`` () =
    raisesWith <@ pop [] @>  (fun e -> <@ e.Message = "Stack underflow" @>)
module OperationTests

open FSharp.Reflection
open Xunit

type TestData() =
  static member ParseOperationTestData =
    [ (Add, "+")
      (Subtract, "-") 
      (Multiply, "*") 
      (Divide, "/") 
      (NoOperation, "") 
    ] |> Seq.map FSharpValue.GetTupleFields

[<Theory>]
[<MemberData("ParseOperationTestData", MemberType=typeof<TestData>)>]
let ``parseOperation returns correct symbol`` (operation: Operation, expected: string) =
    let actual = parseOperation operation 
    Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("0", "0", "0")>]
[<InlineData("1", "0", "1")>]
[<InlineData("0", "1", "1")>]
[<InlineData("-1", "0", "-1")>]
[<InlineData("0", "-1", "-1")>]
[<InlineData("1", "1", "2")>]
[<InlineData("1", "-1", "0")>]
[<InlineData("-1", "1", "0")>]
[<InlineData("-1", "-1", "-2")>]
[<InlineData("2", "3", "5")>]
[<InlineData("2", "-3", "-1")>]
[<InlineData("-2", "3", "1")>]
[<InlineData("-2", "-3", "-5")>]
[<InlineData("3", "2", "5")>]
[<InlineData("3", "-2", "1")>]
[<InlineData("-3", "2", "-1")>]
[<InlineData("-3", "-2", "-5")>]
let ``calculate correctly adds two numbers`` (stored: string, input: string, expected: string) =
    let actual = calculate stored input Add
    Assert.Equal(expected, actual)
        
[<Theory>]
[<InlineData("0", "0", "0")>]
[<InlineData("1", "0", "1")>]
[<InlineData("0", "1", "-1")>]
[<InlineData("-1", "0", "-1")>]
[<InlineData("0", "-1", "1")>]
[<InlineData("1", "1", "0")>]
[<InlineData("1", "-1", "2")>]
[<InlineData("-1", "1", "-2")>]
[<InlineData("-1", "-1", "0")>]
[<InlineData("2", "3", "-1")>]
[<InlineData("2", "-3", "5")>]
[<InlineData("-2", "3", "-5")>]
[<InlineData("-2", "-3", "1")>]
[<InlineData("3", "2", "1")>]
[<InlineData("3", "-2", "5")>]
[<InlineData("-3", "2", "-5")>]
[<InlineData("-3", "-2", "-1")>]
let ``calculate correctly subtracts two numbers`` (stored: string, input: string, expected: string) =
    let actual = calculate stored input Subtract
    Assert.Equal(expected, actual)
    
[<Theory>]
[<InlineData("0", "0", "0")>]
[<InlineData("1", "0", "0")>]
[<InlineData("0", "1", "0")>]
[<InlineData("-1", "0", "0")>]
[<InlineData("0", "-1", "0")>]
[<InlineData("1", "1", "1")>]
[<InlineData("1", "-1", "-1")>]
[<InlineData("-1", "1", "-1")>]
[<InlineData("-1", "-1", "1")>]
[<InlineData("2", "3", "6")>]
[<InlineData("2", "-3", "-6")>]
[<InlineData("-2", "3", "-6")>]
[<InlineData("-2", "-3", "6")>]
[<InlineData("3", "2", "6")>]
[<InlineData("3", "-2", "-6")>]
[<InlineData("-3", "2", "-6")>]
[<InlineData("-3", "-2", "6")>]
let ``calculate correctly multiplies two numbers`` (stored: string, input: string, expected: string) =
    let actual = calculate stored input Multiply
    Assert.Equal(expected, actual)
    
[<Theory>]
[<InlineData("0", "0", "NaN")>]
[<InlineData("1", "0", "Infinity")>]
[<InlineData("0", "1", "0")>]
[<InlineData("-1", "0", "-Infinity")>]
[<InlineData("0", "-1", "0")>]
[<InlineData("1", "1", "1")>]
[<InlineData("1", "-1", "-1")>]
[<InlineData("-1", "1", "-1")>]
[<InlineData("-1", "-1", "1")>]
[<InlineData("2", "3", "0.666666666666667")>]
[<InlineData("2", "-3", "-0.666666666666667")>]
[<InlineData("-2", "3", "-0.666666666666667")>]
[<InlineData("-2", "-3", "0.666666666666667")>]
[<InlineData("3", "2", "1.5")>]
[<InlineData("3", "-2", "-1.5")>]
[<InlineData("-3", "2", "-1.5")>]
[<InlineData("-3", "-2", "1.5")>]
let ``calculate correctly divides two numbers`` (stored: string, input: string, expected: string) =
    let actual = calculate stored input Divide
    Assert.Equal(expected, actual)
    
[<Theory>]
[<InlineData("0", "0", "0")>]
[<InlineData("1", "0", "0")>]
[<InlineData("0", "1", "1")>]
[<InlineData("-1", "0", "0")>]
[<InlineData("0", "-1", "-1")>]
[<InlineData("1", "1", "1")>]
[<InlineData("1", "-1", "-1")>]
[<InlineData("-1", "1", "1")>]
[<InlineData("-1", "-1", "-1")>]
[<InlineData("2", "3", "3")>]
[<InlineData("2", "-3", "-3")>]
[<InlineData("-2", "3", "3")>]
[<InlineData("-2", "-3", "-3")>]
[<InlineData("3", "2", "2")>]
[<InlineData("3", "-2", "-2")>]
[<InlineData("-3", "2", "2")>]
[<InlineData("-3", "-2", "-2")>]
let ``calculate returns the input given no operation`` (stored: string, input: string, expected: string) =
    let actual = calculate stored input NoOperation
    Assert.Equal(expected, actual)

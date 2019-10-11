module CalculatorConverterTests.Calculator.AppendDigitTests

open Xunit

type TestData() =
  static member ActivityTestData =
    [
      [| NoActivity |]
      [| Calculate |]
      [| Operation Add |]
      [| Operation Subtract |]
      [| Operation Multiply |]
      [| Operation Divide |]
      [| OpenParenthesis |]
      [| CloseParenthesis |]
    ]

let assertModelIsCorrect (expectedModel: Model) (actualModel: Model) =
  Assert.Equal(expectedModel.input, actualModel.input)
  Assert.Equal(expectedModel.calculation |> List.fold (+) "", actualModel.calculation |> List.fold (+) "")
  Assert.Equal(expectedModel.lastActivity, actualModel.lastActivity)
  Assert.Equal(expectedModel.calculationResult, actualModel.calculationResult)

[<Fact>]
let ``appendDigit after DigitInput appends digit to input if input is not "0"`` () =
  let model = {input = "1"; calculation = []; lastActivity = DigitInput; calculationResult = 0.0}
  let digit = "1"

  let actualModel = appendDigit model digit
  let expectedModel = {input = "11"; calculation = []; lastActivity = DigitInput; calculationResult = 0.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``appendDigit after DigitInput replaces input with digit if input is "0"`` () =
  let model = {input = "0"; calculation = []; lastActivity = DigitInput; calculationResult = 0.0}
  let digit = "1"

  let actualModel = appendDigit model digit
  let expectedModel = {input = "1"; calculation = []; lastActivity = DigitInput; calculationResult = 0.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``appendDigit after DecimalPointInput appends digit to input`` () =
  let model = {input = "0."; calculation = []; lastActivity = DecimalPointInput; calculationResult = 0.0}
  let digit = "1"

  let actualModel = appendDigit model digit
  let expectedModel = {input = "0.1"; calculation = []; lastActivity = DigitInput; calculationResult = 0.0}
  assertModelIsCorrect expectedModel actualModel

[<Theory>]
[<MemberData("ActivityTestData", MemberType=typeof<TestData>)>]
let ``appendDigit after neither DigitInput or DecimalPointInput replaces input with digit`` (activity: Activity) =
  let model = {input = "2"; calculation = []; lastActivity = activity; calculationResult = 0.0}
  let digit = "1"

  let actualModel = appendDigit model digit
  let expectedModel = {input = "1"; calculation = []; lastActivity = DigitInput; calculationResult = 0.0}
  assertModelIsCorrect expectedModel actualModel


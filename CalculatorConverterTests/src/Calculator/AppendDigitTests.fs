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
  let model = {input = "1"; calculation = []; lastActivity = DigitInput; calculationResult = None}
  let digit = "1"

  let actualModel = appendDigit model digit
  let expectedModel = {input = "11"; calculation = []; lastActivity = DigitInput; calculationResult = None}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``appendDigit after DigitInput replaces input with digit if input is "0"`` () =
  let model = {input = "0"; calculation = []; lastActivity = DigitInput; calculationResult = None}
  let digit = "1"

  let actualModel = appendDigit model digit
  let expectedModel = {input = "1"; calculation = []; lastActivity = DigitInput; calculationResult = None}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``appendDigit after DecimalPointInput appends digit to input`` () =
  let model = {input = "0."; calculation = []; lastActivity = DecimalPointInput; calculationResult = None}
  let digit = "1"

  let actualModel = appendDigit model digit
  let expectedModel = {input = "0.1"; calculation = []; lastActivity = DigitInput; calculationResult = None}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``appendDigit after InvertSign after AppendDigit appends digit to input`` () =
  let modelOne = {input = "1"; calculation = []; lastActivity = DigitInput; calculationResult = None}
  let actualModelTwo = invertSign modelOne
  let expectedModelTwo = {input = "-1"; calculation = []; lastActivity = InvertSign; calculationResult = None}
  assertModelIsCorrect expectedModelTwo actualModelTwo

  let digit = "1"
  let actualModel = appendDigit actualModelTwo digit
  let expectedModel = {input = "-11"; calculation = []; lastActivity = DigitInput; calculationResult = None}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``appendDigit after InvertSign after Calculate replaces input`` () =
  let modelOne = {input = "1"; calculation = []; lastActivity = Calculate; calculationResult = Some 1.0}
  let actualModelTwo = invertSign modelOne
  let expectedModelTwo = {input = "-1"; calculation = []; lastActivity = InvertSign; calculationResult = Some 1.0}
  assertModelIsCorrect expectedModelTwo actualModelTwo

  let digit = "1"
  let actualModel = appendDigit actualModelTwo digit
  let expectedModel = {input = "1"; calculation = []; lastActivity = DigitInput; calculationResult = None}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``appendDigit after Calculate replaces input`` () =
  let model = {input = "5"; calculation = []; lastActivity = Calculate; calculationResult = Some 5.0}
  let digit = "1"

  let actualModel = appendDigit model digit
  let expectedModel = {input = "1"; calculation = []; lastActivity = DigitInput; calculationResult = None}
  assertModelIsCorrect expectedModel actualModel

[<Theory>]
[<MemberData("ActivityTestData", MemberType=typeof<TestData>)>]
let ``appendDigit after neither DigitInput or DecimalPointInput replaces input with digit`` (activity: Activity) =
  let model = {input = "2"; calculation = []; lastActivity = activity; calculationResult = None}
  let digit = "1"

  let actualModel = appendDigit model digit
  let expectedModel = {input = "1"; calculation = []; lastActivity = DigitInput; calculationResult = None}
  assertModelIsCorrect expectedModel actualModel


module CalculatorConverterTests.Calculator.AppendDecimalPointTests

open Xunit

type TestData() =
  static member ActivityTestData =
    [
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
let ``appendDecimalPoint after DecimalPointInput does not append a decimal point`` () =
  let model = {input = "0."; calculation = []; lastActivity = DecimalPointInput; calculationResult = 0.0}

  let actualModel = appendDecimalPoint model
  let expectedModel = {input = "0."; calculation = []; lastActivity = DecimalPointInput; calculationResult = 0.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``appendDecimalPoint after DigitInput does not append a decimal point if a decimal point already exists`` () =
  let model = {input = "0.0"; calculation = []; lastActivity = DigitInput; calculationResult = 0.0}

  let actualModel = appendDecimalPoint model
  let expectedModel = {input = "0.0"; calculation = []; lastActivity = DigitInput; calculationResult = 0.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``appendDecimalPoint after NoActivity appends a decimal point`` () =
  let model = {input = "0"; calculation = []; lastActivity = NoActivity; calculationResult = 0.0}

  let actualModel = appendDecimalPoint model
  let expectedModel = {input = "0."; calculation = []; lastActivity = DecimalPointInput; calculationResult = 0.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``appendDecimalPoint after DigitInput appends a decimal point if a decimal point does not already exist`` () =
  let model = {input = "1"; calculation = []; lastActivity = DigitInput; calculationResult = 0.0}

  let actualModel = appendDecimalPoint model
  let expectedModel = {input = "1."; calculation = []; lastActivity = DecimalPointInput; calculationResult = 0.0}
  assertModelIsCorrect expectedModel actualModel

[<Theory>]
[<MemberData("ActivityTestData", MemberType=typeof<TestData>)>]
let ``appendDecimalPoint after an activity other than DigitInput or DecimalPointInput replaces input with "0."`` (activity: Activity) =
  let model = {input = "1"; calculation = []; lastActivity = activity; calculationResult = 0.0}

  let actualModel = appendDecimalPoint model
  let expectedModel = {input = "0."; calculation = []; lastActivity = DecimalPointInput; calculationResult = 0.0}
  assertModelIsCorrect expectedModel actualModel


module CalculatorConverterTests.Calculator.calculateResultTests

open Xunit

let assertModelIsCorrect (expectedModel: Model) (actualModel: Model) =
  Assert.Equal(expectedModel.input, actualModel.input)
  Assert.Equal(expectedModel.calculation |> List.fold (+) "", actualModel.calculation |> List.fold (+) "")
  Assert.Equal(expectedModel.lastActivity, actualModel.lastActivity)
  Assert.Equal(expectedModel.calculationResult, actualModel.calculationResult)

[<Fact>]
let ``calculate result updates calculation result given an empty calculation`` () =
  let model = {input = "1"; calculation = []; lastActivity = DigitInput; calculationResult = 0.0}

  let actualModel = calculateResult model
  let expectedModel = {input = "1"; calculation = []; lastActivity = Calculate; calculationResult = 1.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``calculate result updates calculation result if last input was a decimal point`` () =
  let model = {input = "1."; calculation = []; lastActivity = DecimalPointInput; calculationResult = 0.0}

  let actualModel = calculateResult model
  let expectedModel = {input = "1"; calculation = []; lastActivity = Calculate; calculationResult = 1.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``calculate result updates calculation and calculation result if last input was an operation`` () =
  let model = {input = "3"; calculation = ["3"; "+"]; lastActivity = Operation Add; calculationResult = 0.0}

  let actualModel = calculateResult model
  let expectedModel = {input = "6"; calculation = []; lastActivity = Calculate; calculationResult = 6.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``calculate result updates calculation and calculation result if last input was a DigitInput`` () =
  let model = {input = "2"; calculation = ["3"; "+"]; lastActivity = DigitInput; calculationResult = 0.0}

  let actualModel = calculateResult model
  let expectedModel = {input = "5"; calculation = []; lastActivity = Calculate; calculationResult = 5.0}
  assertModelIsCorrect expectedModel actualModel




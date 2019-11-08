module CalculatorConverterTests.Calculator.ReplaceInputTests

open Xunit

let assertModelIsCorrect (expectedModel: Model) (actualModel: Model) =
  Assert.Equal(expectedModel.input, actualModel.input)
  Assert.Equal(expectedModel.calculation |> List.fold (+) "", actualModel.calculation |> List.fold (+) "")
  Assert.Equal(expectedModel.lastActivity, actualModel.lastActivity)
  Assert.Equal(expectedModel.calculationResult, actualModel.calculationResult)

[<Fact>]
let ``replaceInput replaces the input with the given value`` () =
  let model = {input = "1"; calculation = []; lastActivity = DigitInput; calculationResult = None}
  let newValue = "123"

  let actualModel = replaceInput model newValue
  let expectedModel = {input = newValue; calculation = []; lastActivity = DigitInput; calculationResult = None}
  assertModelIsCorrect expectedModel actualModel

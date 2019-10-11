module CalculatorConverterTests.Calculator.ClearEntryTests

open Xunit

let assertModelIsCorrect (expectedModel: Model) (actualModel: Model) =
  Assert.Equal(expectedModel.input, actualModel.input)
  Assert.Equal(expectedModel.calculation |> List.fold (+) "", actualModel.calculation |> List.fold (+) "")
  Assert.Equal(expectedModel.lastActivity, actualModel.lastActivity)
  Assert.Equal(expectedModel.calculationResult, actualModel.calculationResult)

[<Fact>]
let ``clearEntry sets the input to "0" if it includes a decimal`` () =
  let model = {input = "1.1"; calculation = []; lastActivity = DigitInput; calculationResult = 0.0}

  let actualModel = clearEntry model
  let expectedModel = {input = "0"; calculation = []; lastActivity = NoActivity; calculationResult = 0.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``clearEntry sets the input to "0" if it does not include a decimal`` () =
  let model = {input = "1"; calculation = []; lastActivity = DigitInput; calculationResult = 0.0}

  let actualModel = clearEntry model
  let expectedModel = {input = "0"; calculation = []; lastActivity = NoActivity; calculationResult = 0.0}
  assertModelIsCorrect expectedModel actualModel

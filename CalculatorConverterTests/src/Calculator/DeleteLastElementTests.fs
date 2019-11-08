module CalculatorConverterTests.Calculator.DeleteLastElementTests

open Xunit

let assertModelIsCorrect (expectedModel: Model) (actualModel: Model) =
  Assert.Equal(expectedModel.input, actualModel.input)
  Assert.Equal(expectedModel.calculation |> List.fold (+) "", actualModel.calculation |> List.fold (+) "")
  Assert.Equal(expectedModel.lastActivity, actualModel.lastActivity)
  Assert.Equal(expectedModel.calculationResult, actualModel.calculationResult)

[<Fact>]
let ``deleteLastElement deletes a decimal point if it is the last element`` () =
  let model = {input = "1."; calculation = []; lastActivity = DecimalPointInput; calculationResult = None}

  let actualModel = deleteLastElement model
  let expectedModel = {input = "1"; calculation = []; lastActivity = DigitInput; calculationResult = None}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``deleteLastElement deletes the last digit if there is more than 1`` () =
  let model = {input = "12"; calculation = []; lastActivity = DigitInput; calculationResult = None}

  let actualModel = deleteLastElement model
  let expectedModel = {input = "1"; calculation = []; lastActivity = DigitInput; calculationResult = None}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``deleteLastElement sets input to 0 if deleting the only element`` () =
  let model = {input = "1"; calculation = []; lastActivity = DigitInput; calculationResult = None}

  let actualModel = deleteLastElement model
  let expectedModel = {input = "0"; calculation = []; lastActivity = DigitInput; calculationResult = None}
  assertModelIsCorrect expectedModel actualModel

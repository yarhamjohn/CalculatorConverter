module CalculatorConverterTests.Calculator.PerformOperationTests

open Xunit

let assertModelIsCorrect (expectedModel: Model) (actualModel: Model) =
  Assert.Equal(expectedModel.input, actualModel.input)
  Assert.Equal(expectedModel.calculation |> List.fold (+) "", actualModel.calculation |> List.fold (+) "")
  Assert.Equal(expectedModel.lastActivity, actualModel.lastActivity)
  Assert.Equal(expectedModel.calculationResult, actualModel.calculationResult)

[<Fact>]
let ``performOperation does nothing if last activity was the same operation`` () =
  let model = {input = "1"; calculation = ["1"; "+"]; lastActivity = Operation Add; calculationResult = 0.0}

  let actualModel = performOperation model Add
  let expectedModel = {input = "1"; calculation = ["1"; "+"]; lastActivity = Operation Add; calculationResult = 0.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``performOperation replaces the last operation if the last activity was a different operation`` () =
  let model = {input = "1"; calculation = ["1"; "+"]; lastActivity = Operation Add; calculationResult = 0.0}

  let actualModel = performOperation model Subtract
  let expectedModel = {input = "1"; calculation = ["1"; "-"]; lastActivity = Operation Subtract; calculationResult = 0.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``performOperation updates the calculation given no previous activity`` () =
  let model = {input = "0"; calculation = []; lastActivity = NoActivity; calculationResult = 0.0}

  let actualModel = performOperation model Add
  let expectedModel = {input = "0"; calculation = ["0"; "+"]; lastActivity = Operation Add; calculationResult = 0.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``performOperation updates the calculation and input when the last activity was DecimalPointInput`` () =
  let model = {input = "1."; calculation = []; lastActivity = DecimalPointInput; calculationResult = 0.0}

  let actualModel = performOperation model Add
  let expectedModel = {input = "1"; calculation = ["1"; "+"]; lastActivity = Operation Add; calculationResult = 0.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``performOperation updates the calculation when the last activity was DigitInput`` () =
  let model = {input = "1"; calculation = []; lastActivity = DigitInput; calculationResult = 0.0}

  let actualModel = performOperation model Add
  let expectedModel = {input = "1"; calculation = ["1"; "+"]; lastActivity = Operation Add; calculationResult = 0.0}
  assertModelIsCorrect expectedModel actualModel

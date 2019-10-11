module CalculatorConverterTests.Calculator.InvertSignTests

open Xunit

let assertModelIsCorrect (expectedModel: Model) (actualModel: Model) =
  Assert.Equal(expectedModel.input, actualModel.input)
  Assert.Equal(expectedModel.calculation |> List.fold (+) "", actualModel.calculation |> List.fold (+) "")
  Assert.Equal(expectedModel.lastActivity, actualModel.lastActivity)
  Assert.Equal(expectedModel.calculationResult, actualModel.calculationResult)

[<Fact>]
let ``invertSign does nothing if the input is "0"`` () =
  let model = {input = "0"; calculation = []; lastActivity = DigitInput; calculationResult = 0.0}

  let actualModel = invertSign model
  let expectedModel = {input = "0"; calculation = []; lastActivity = DigitInput; calculationResult = 0.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``invertSign makes "0.0" negative`` () =
  let model = {input = "0.0"; calculation = []; lastActivity = DigitInput; calculationResult = 0.0}

  let actualModel = invertSign model
  let expectedModel = {input = "-0.0"; calculation = []; lastActivity = DigitInput; calculationResult = 0.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``invertSign makes "-0.0" positive`` () =
  let model = {input = "-0.0"; calculation = []; lastActivity = DigitInput; calculationResult = 0.0}

  let actualModel = invertSign model
  let expectedModel = {input = "0.0"; calculation = []; lastActivity = DigitInput; calculationResult = 0.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``invertSign makes positive numbers negative`` () =
  let model = {input = "1"; calculation = []; lastActivity = DigitInput; calculationResult = 0.0}

  let actualModel = invertSign model
  let expectedModel = {input = "-1"; calculation = []; lastActivity = DigitInput; calculationResult = 0.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``invertSign makes negative numbers positive`` () =
  let model = {input = "-1"; calculation = []; lastActivity = DigitInput; calculationResult = 0.0}

  let actualModel = invertSign model
  let expectedModel = {input = "1"; calculation = []; lastActivity = DigitInput; calculationResult = 0.0}
  assertModelIsCorrect expectedModel actualModel


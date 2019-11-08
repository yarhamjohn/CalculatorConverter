module CalculatorConverterTests.Calculator.InvertSignTests

open Xunit

let assertModelIsCorrect (expectedModel: Model) (actualModel: Model) =
  Assert.Equal(expectedModel.input, actualModel.input)
  Assert.Equal(expectedModel.calculation |> List.fold (+) "", actualModel.calculation |> List.fold (+) "")
  Assert.Equal(expectedModel.lastActivity, actualModel.lastActivity)
  Assert.Equal(expectedModel.calculationResult, actualModel.calculationResult)

[<Fact>]
let ``invertSign does nothing if the input is "0"`` () =
  let model = {input = "0"; calculation = []; lastActivity = DigitInput; calculationResult = None}

  let actualModel = invertSign model
  let expectedModel = {input = "0"; calculation = []; lastActivity = InvertSign; calculationResult = None}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``invertSign makes "None" negative`` () =
  let model = {input = "None"; calculation = []; lastActivity = DigitInput; calculationResult = None}

  let actualModel = invertSign model
  let expectedModel = {input = "-None"; calculation = []; lastActivity = InvertSign; calculationResult = None}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``invertSign makes "-None" positive`` () =
  let model = {input = "-None"; calculation = []; lastActivity = DigitInput; calculationResult = None}

  let actualModel = invertSign model
  let expectedModel = {input = "None"; calculation = []; lastActivity = InvertSign; calculationResult = None}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``invertSign makes positive numbers negative`` () =
  let model = {input = "1"; calculation = []; lastActivity = DigitInput; calculationResult = None}

  let actualModel = invertSign model
  let expectedModel = {input = "-1"; calculation = []; lastActivity = InvertSign; calculationResult = None}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``invertSign makes negative numbers positive`` () =
  let model = {input = "-1"; calculation = []; lastActivity = DigitInput; calculationResult = None}

  let actualModel = invertSign model
  let expectedModel = {input = "1"; calculation = []; lastActivity = InvertSign; calculationResult = None}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``invertSign inverts number after calculate`` () =
  let model = {input = "1"; calculation = []; lastActivity = Calculate; calculationResult = Some 1.0}

  let actualModel = invertSign model
  let expectedModel = {input = "-1"; calculation = []; lastActivity = InvertSign; calculationResult = Some 1.0}
  assertModelIsCorrect expectedModel actualModel

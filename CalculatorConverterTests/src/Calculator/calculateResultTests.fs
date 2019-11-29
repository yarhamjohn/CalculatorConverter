module CalculatorConverterTests.Calculator.calculateResultTests

open Xunit

let assertModelIsCorrect (expectedModel: Model) (actualModel: Model) =
  Assert.Equal(expectedModel.input, actualModel.input)
  Assert.Equal(expectedModel.calculation |> List.fold (+) "", actualModel.calculation |> List.fold (+) "")
  Assert.Equal(expectedModel.lastActivity, actualModel.lastActivity)
  Assert.Equal(expectedModel.calculationResult, actualModel.calculationResult)

[<Fact>]
let ``calculate result updates calculation result given an empty calculation`` () =
  let model = {input = "1"; calculation = []; lastActivity = DigitInput; calculationResult = None}

  let actualModel = calculateResult model
  let expectedModel = {input = "1"; calculation = []; lastActivity = Calculate; calculationResult = Some 1.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``calculate result updates calculation result if last input was a decimal point given an empty calculation`` () =
  let model = {input = "1."; calculation = []; lastActivity = DecimalPointInput; calculationResult = None}

  let actualModel = calculateResult model
  let expectedModel = {input = "1"; calculation = []; lastActivity = Calculate; calculationResult = Some 1.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``calculate result updates calculation result if last input was a decimal point given a non-empty calculation`` () =
  let model = {input = "1."; calculation = ["1"; "+"]; lastActivity = DecimalPointInput; calculationResult = None}

  let actualModel = calculateResult model
  let expectedModel = {input = "2"; calculation = []; lastActivity = Calculate; calculationResult = Some 2.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``calculate result updates calculation and calculation result if last input was an operation`` () =
  let model = {input = "3"; calculation = ["3"; "+"]; lastActivity = Operation Add; calculationResult = None}

  let actualModel = calculateResult model
  let expectedModel = {input = "6"; calculation = []; lastActivity = Calculate; calculationResult = Some 6.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``calculate result updates calculation and calculation result if last input was a DigitInput`` () =
  let model = {input = "2"; calculation = ["3"; "+"]; lastActivity = DigitInput; calculationResult = None}

  let actualModel = calculateResult model
  let expectedModel = {input = "5"; calculation = []; lastActivity = Calculate; calculationResult = Some 5.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``calculate result updates calculation and calculation result if last input was a DigitInput for a multi-part calculation`` () =
  let model = {input = "3"; calculation = ["1"; "+"; "2"; "+"]; lastActivity = DigitInput; calculationResult = None}

  let actualModel = calculateResult model
  let expectedModel = {input = "6"; calculation = []; lastActivity = Calculate; calculationResult = Some 6.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``calculate result takes simple precedence into account`` () =
  let model = {input = "3"; calculation = ["1"; "+"; "2"; "*"]; lastActivity = DigitInput; calculationResult = None}

  let actualModel = calculateResult model
  let expectedModel = {input = "7"; calculation = []; lastActivity = Calculate; calculationResult = Some 7.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``calculate result takes multiple precedence into account with adjacent top level operators`` () =
  let model = {input = "2"; calculation = ["1"; "+"; "2"; "*"; "3"; "/"]; lastActivity = DigitInput; calculationResult = None}

  let actualModel = calculateResult model
  let expectedModel = {input = "4"; calculation = []; lastActivity = Calculate; calculationResult = Some 4.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``calculate result takes multiple precedence into account with adjacent low level operators`` () =
  let model = {input = "4"; calculation = ["1"; "+"; "2"; "+"; "3"; "*"]; lastActivity = DigitInput; calculationResult = None}

  let actualModel = calculateResult model
  let expectedModel = {input = "15"; calculation = []; lastActivity = Calculate; calculationResult = Some 15.0}
  assertModelIsCorrect expectedModel actualModel




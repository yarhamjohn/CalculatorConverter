module CalculatorConverterTests.Calculator.PerformOperationTests

open Xunit

let assertModelIsCorrect (expectedModel: Model) (actualModel: Model) =
  Assert.Equal(expectedModel.input, actualModel.input)
  Assert.Equal(expectedModel.calculation |> List.fold (+) "", actualModel.calculation |> List.fold (+) "")
  Assert.Equal(expectedModel.lastActivity, actualModel.lastActivity)
  Assert.Equal(expectedModel.calculationResult, actualModel.calculationResult)

[<Fact>]
let ``performOperation does nothing if last activity was the same operation`` () =
  let model = {input = "1"; calculation = ["1"; "+"]; lastActivity = Operation Add; calculationResult = None}

  let actualModel = performOperation model Add
  let expectedModel = {input = "1"; calculation = ["1"; "+"]; lastActivity = Operation Add; calculationResult = None}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``performOperation replaces the last operation if the last activity was a different operation`` () =
  let model = {input = "1"; calculation = ["1"; "+"]; lastActivity = Operation Add; calculationResult = None}

  let actualModel = performOperation model Subtract
  let expectedModel = {input = "1"; calculation = ["1"; "-"]; lastActivity = Operation Subtract; calculationResult = None}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``performOperation updates the calculation given no previous activity`` () =
  let model = {input = "0"; calculation = []; lastActivity = NoActivity; calculationResult = None}

  let actualModel = performOperation model Add
  let expectedModel = {input = "0"; calculation = ["0"; "+"]; lastActivity = Operation Add; calculationResult = None}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``performOperation updates the calculation and input when the last activity was DecimalPointInput`` () =
  let model = {input = "1."; calculation = []; lastActivity = DecimalPointInput; calculationResult = None}

  let actualModel = performOperation model Add
  let expectedModel = {input = "1"; calculation = ["1"; "+"]; lastActivity = Operation Add; calculationResult = None}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``performOperation updates the calculation when the last activity was DigitInput`` () =
  let model = {input = "1"; calculation = []; lastActivity = DigitInput; calculationResult = None}

  let actualModel = performOperation model Add
  let expectedModel = {input = "1"; calculation = ["1"; "+"]; lastActivity = Operation Add; calculationResult = None}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``performOperation updates the calculation when the last activity was Calculate`` () =
  let model = {input = "1"; calculation = []; lastActivity = Calculate; calculationResult = Some 1.0}

  let actualModel = performOperation model Add
  let expectedModel = {input = "1"; calculation = ["1"; "+"]; lastActivity = Operation Add; calculationResult = None}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``performOperation updates the calculation when the last activity was InvertSign`` () =
  let model = {input = "-1"; calculation = []; lastActivity = InvertSign; calculationResult = None}

  let actualModel = performOperation model Add
  let expectedModel = {input = "-1"; calculation = ["-1"; "+"]; lastActivity = Operation Add; calculationResult = None}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``performOperation performs a partial calculation given a precedent operator`` () =
  let model = {input = "3"; calculation = ["2"; "+"]; lastActivity = DigitInput; calculationResult = None}

  let actualModel = performOperation model Subtract
  let expectedModel = {input = "5"; calculation = ["2"; "+"; "3"; "-"]; lastActivity = Operation Subtract; calculationResult = Some 5.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``performOperation does not perform a calculation given a non-precedent operator`` () =
  let model = {input = "3"; calculation = ["2"; "+"]; lastActivity = DigitInput; calculationResult = None}

  let actualModel = performOperation model Multiply
  let expectedModel = {input = "3"; calculation = ["2"; "+"; "3"; "*"]; lastActivity = Operation Multiply; calculationResult = None}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``performOperation does performs correct calculation given a variety of operators`` () =
  let model = {input = "4"; calculation = ["2"; "+"; "3"; "*"]; lastActivity = DigitInput; calculationResult = None}

  let actualModel = performOperation model Add
  let expectedModel = {input = "14"; calculation = ["2"; "+"; "3"; "*"; "4"; "+"]; lastActivity = Operation Add; calculationResult = Some 14.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``performOperation does performs correct calculation given a different variety of operators`` () =
  let model = {input = "4"; calculation = ["2"; "+"; "3"; "*"]; lastActivity = DigitInput; calculationResult = None}

  let actualModel = performOperation model Multiply
  let expectedModel = {input = "12"; calculation = ["2"; "+"; "3"; "*"; "4"; "*"]; lastActivity = Operation Add; calculationResult = Some 12.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``performOperation does performs correct calculation given a different variety of operators 2`` () =
  let model = {input = "5"; calculation = ["2"; "+"; "3"; "*"; "4"; "*"]; lastActivity = DigitInput; calculationResult = None}

  let actualModel = performOperation model Multiply
  let expectedModel = {input = "60"; calculation = ["2"; "+"; "3"; "*"; "4"; "*"; "5"; "*"]; lastActivity = Operation Add; calculationResult = Some 60.0}
  assertModelIsCorrect expectedModel actualModel

[<Fact>]
let ``performOperation does performs correct calculation given a different variety of operators 3`` () =
  let model = {input = "6"; calculation = ["2"; "*"; "3"; "*"; "6"; "+"; "2"; "*"; "3"; "*"]; lastActivity = DigitInput; calculationResult = None}

  let actualModel = performOperation model Add
  let expectedModel = {input = "72"; calculation = ["2"; "*"; "3"; "*"; "6"; "+"; "2"; "*"; "3"; "*"; "6"; "+"]; lastActivity = Operation Add; calculationResult = Some 72.0}
  assertModelIsCorrect expectedModel actualModel

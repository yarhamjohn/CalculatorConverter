module CalculatorConverterTests.Calculator.OperationTests

open Xunit

[<Fact>]
let ``parseOperation returns the correct result`` () =
    Assert.Equal(parseOperation Add, "+")
    Assert.Equal(parseOperation Subtract, "-")
    Assert.Equal(parseOperation Multiply, "*")
    Assert.Equal(parseOperation Divide, "/")

[<Fact>]
let ``getOperation returns the correct result`` () =
    Assert.Equal(getOperation "+", Add)
    Assert.Equal(getOperation "-", Subtract)
    Assert.Equal(getOperation "*", Multiply)
    Assert.Equal(getOperation "/", Divide)

[<Fact>]
let ``Add never takes precedence`` () =
    Assert.Equal(Add, getPrecedentOperator Add Add)
    Assert.Equal(Add, getPrecedentOperator Add Subtract)
    Assert.Equal(Multiply, getPrecedentOperator Add Multiply)
    Assert.Equal(Divide, getPrecedentOperator Add Divide)

[<Fact>]
let ``Subtract never takes precedence`` () =
    Assert.Equal(Subtract, getPrecedentOperator Subtract Add)
    Assert.Equal(Subtract, getPrecedentOperator Subtract Subtract)
    Assert.Equal(Multiply, getPrecedentOperator Subtract Multiply)
    Assert.Equal(Divide, getPrecedentOperator Subtract Divide)

[<Fact>]
let ``Multiply takes precedence over Add and Subtract`` () =
    Assert.Equal(Multiply, getPrecedentOperator Multiply Add)
    Assert.Equal(Multiply, getPrecedentOperator Multiply Subtract)
    Assert.Equal(Multiply, getPrecedentOperator Multiply Multiply)
    Assert.Equal(Multiply, getPrecedentOperator Multiply Divide)

[<Fact>]
let ``Divide takes precedence over Add and Subtract`` () =
    Assert.Equal(Divide, getPrecedentOperator Divide Add)
    Assert.Equal(Divide, getPrecedentOperator Divide Subtract)
    Assert.Equal(Divide, getPrecedentOperator Divide Multiply)
    Assert.Equal(Divide, getPrecedentOperator Divide Divide)

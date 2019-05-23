module ActivityTests

open Xunit

[<Theory>]
[<InlineData("", "1", "1")>]
let ``appendDigitToInput returns the digit when given an empty input`` (input: string, digit: string, expected: string) =
  let actual = appendDigitToInput input digit
  Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("0", "1", "1")>]
[<InlineData("00", "1", "1")>]
[<InlineData("0", "0", "0")>]
[<InlineData("00", "0", "0")>]
let ``appendDigitToInput returns the digit when given an input containing only 0`` (input: string, digit: string, expected: string) =
  let actual = appendDigitToInput input digit
  Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("1", "1", "11")>]
[<InlineData("0.", "1", "0.1")>]
[<InlineData("0.1", "1", "0.11")>]
[<InlineData("1.1", "1", "1.11")>]
let ``appendDigitToInput appends the digit when given an input without leading 0`` (input: string, digit: string, expected: string) =
  let actual = appendDigitToInput input digit
  Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("01", "1", "11")>]
[<InlineData("00.", "1", "0.1")>]
[<InlineData("00.1", "1", "0.11")>]
[<InlineData("01.1", "1", "1.11")>]
let ``appendDigitToInput appends the digit when given an input with leading 0`` (input: string, digit: string, expected: string) =
  let actual = appendDigitToInput input digit
  Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("", "0.")>]
let ``appendDecimalPointToInput returns the correct result when given an empty input`` (input: string, expected: string) =
  let actual = appendDecimalPointToInput input
  Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("0", "0.")>]
[<InlineData("1", "1.")>]
let ``appendDecimalPointToInput appends a decimal point to an input with no decimal point`` (input: string, expected: string) =
  let actual = appendDecimalPointToInput input
  Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("0.", "0.")>]
[<InlineData("1.", "1.")>]
[<InlineData("0.0", "0.0")>]
[<InlineData("1.1", "1.1")>]
[<InlineData(".", "0.")>]
[<InlineData(".0", "0.0")>]
[<InlineData(".1", "0.1")>]
let ``appendDecimalPointToInput does not append a decimal pointn to an input with a decimal point`` (input: string, expected: string) =
  let actual = appendDecimalPointToInput input
  Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("", "0")>]
let ``deleteFromInput returns 0 when given an empty input`` (input: string, expected: string) =
  let actual = deleteFromInput input
  Assert.Equal(expected, actual)
  
[<Theory>]
[<InlineData("0", "0")>]
[<InlineData("1", "0")>]
[<InlineData(".", "0")>]
[<InlineData("-", "0")>]
let ``deleteFromInput returns 0 when deleting the only digit`` (input: string, expected: string) =
  let actual = deleteFromInput input
  Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("0.", "0")>]
[<InlineData("1.", "1")>]
[<InlineData("-0.", "-0")>]
[<InlineData("-1.", "-1")>]
let ``deleteFromInput removes a trailing decimal point`` (input: string, expected: string) =
  let actual = deleteFromInput input
  Assert.Equal(expected, actual)
  
[<Theory>]
[<InlineData("10", "1")>]
[<InlineData("0.0", "0.")>]
[<InlineData("0.1", "0.")>]
[<InlineData("1.1", "1.")>]
[<InlineData("-0.0", "-0.")>]
[<InlineData("-1.0", "-1.")>]
let ``deleteFromInput removes the final digit`` (input: string, expected: string) =
  let actual = deleteFromInput input
  Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("-0", "0")>]
[<InlineData("-1", "0")>]
[<InlineData(".0", "0.")>]
[<InlineData(".1", "0.")>]
let ``deleteFromInput returns 0 when no digit is left`` (input: string, expected: string) =
  let actual = deleteFromInput input
  Assert.Equal(expected, actual)
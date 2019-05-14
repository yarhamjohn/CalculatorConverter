[<AutoOpen>]
module Activity

type Activity =
  | Operation
  | Calculate
  | DigitInput
  | DecimalPointInput
  | NoActivity

let appendDigitToInput (input: string) (digit: string) =
  if input.TrimStart('0').Length = 0 then digit else input + digit

let appendDecimalPointToInput (input: string) =
  if input.Contains(".") || input.Length = 0 then input else input + "."

let deleteFromInput (input: string) =
  if input.Length = 1
  then "0"
  else
    let result = input.Substring(0, input.Length - 1)
    if result = "-" then "0" else result

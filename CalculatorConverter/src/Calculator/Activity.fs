[<AutoOpen>]
module Activity

type Activity =
  | Operation
  | Calculate
  | DigitInput
  | DecimalPointInput
  | OpenParenthesis
  | CloseParenthesis
  | NoActivity

let appendDigitToInput (input: string) (digit: string) =
  let trimmedInput = input.TrimStart('0')
  
  if trimmedInput.Length = 0 then digit
  elif trimmedInput.Substring(0, 1) = "." then "0" + trimmedInput + digit
  else trimmedInput + digit

let appendDecimalPointToInput (input: string) =
  if input.Length = 0 then "0."
  elif input.Substring(0, 1) = "." then "0" + input
  elif input.Contains(".") then input
  else input + "."

let deleteFromInput (input: string) =
  if input.Length <= 1 then "0"
  else
    let result = input.Substring(0, input.Length - 1)
    if result = "-" then "0"
    elif result = "." then "0."
    else result

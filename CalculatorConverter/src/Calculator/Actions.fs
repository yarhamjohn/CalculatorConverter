[<AutoOpen>]
module Actions

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
  
let add (leftSide: float) (rightSide: float) =
  leftSide + rightSide
  
let subtract (leftSide: float) (rightSide: float) =
  leftSide - rightSide

let multiply (leftSide: float) (rightSide: float) =
  leftSide * rightSide
  
let divide (leftSide: float) (rightSide: float) =
  leftSide / rightSide
  


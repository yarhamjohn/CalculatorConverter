[<AutoOpen>]
module Actions

//TODO: shouldn't append after pressing equals or multiple actions as shows result
let appendDigitToInput (input: string) (digit: string) =
  if input.TrimStart('0').Length = 0 then digit else input + digit

let appendDecimalPointToInput (input: string) =
  if input.Contains(".") || input.Length = 0 then input else input + "."

let deleteFromInput (input: string) =
  if input.Length = 1 then "0" else input.Substring(0, input.Length - 1)
  
let private add (leftSide: float) (rightSide: float) =
  leftSide + rightSide
  
let private substract (leftSide: float) (rightSide: float) =
  leftSide - rightSide

let private multiply (leftSide: float) (rightSide: float) =
  leftSide * rightSide
  
let private divide (leftSide: float) (rightSide: float) =
  leftSide / rightSide
  
let calculate (action: string) (stored: string) (input: string) =
  let leftSide = float stored
  let rightSide = float input

  match action with
  | "+" -> add leftSide rightSide |> string
  | "-" -> substract leftSide rightSide |> string
  | "*" -> multiply leftSide rightSide |> string
  | "/" -> divide leftSide rightSide |> string
  | _ -> rightSide |> string

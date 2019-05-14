[<AutoOpen>]
module Operation

type Operation =
  | Add
  | Subtract
  | Multiply
  | Divide
  | NoOperation
  
let parseOperation = function
  | Add -> "+"
  | Subtract -> "-"
  | Multiply -> "*"
  | Divide -> "/"
  | _ -> ""
  
let private add (leftSide: float) (rightSide: float) =
  leftSide + rightSide
  
let private subtract (leftSide: float) (rightSide: float) =
  leftSide - rightSide

let private multiply (leftSide: float) (rightSide: float) =
  leftSide * rightSide
  
let private divide (leftSide: float) (rightSide: float) =
  leftSide / rightSide

let calculate (stored: string) (input: string) (operation: Operation) =
  let leftSide = float stored
  let rightSide = float input

  match operation with
  | Add -> add leftSide rightSide |> string
  | Subtract -> subtract leftSide rightSide |> string
  | Multiply -> multiply leftSide rightSide |> string
  | Divide -> divide leftSide rightSide |> string
  | _ -> rightSide |> string
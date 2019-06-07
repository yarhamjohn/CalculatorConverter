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

let getOperation = function
  | "+" -> Add
  | "-" -> Subtract
  | "*" -> Multiply
  | "/" -> Divide
  | _ -> NoOperation

let private add (leftSide: float) (rightSide: float) =
  leftSide + rightSide
  
let private subtract (leftSide: float) (rightSide: float) =
  leftSide - rightSide

let private multiply (leftSide: float) (rightSide: float) =
  leftSide * rightSide
  
let private divide (leftSide: float) (rightSide: float) =
  leftSide / rightSide

//TODO: complete this method...
let calculate (calculation: string list) =
  []

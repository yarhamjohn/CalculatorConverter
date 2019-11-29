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

let getPrecedentOperator (first: Operation) (second: Operation) =
  match first with
  | Multiply | Divide -> first
  | _ -> match second with | Multiply | Divide -> second | _ -> first

let isPrecedent (first: Operation) (second: Operation) =
  first = getPrecedentOperator first second

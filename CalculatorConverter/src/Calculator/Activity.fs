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

let appendDigit (input: string list) (digit: string) =
  List.append input [digit]

let appendDecimalPoint (input: string list) =
  if List.contains "." input then input else List.append input ["."]

let evaluateInput (input: string list) =
  String.concat "" input |> float

let serializeInput (input: string list) =
  evaluateInput input |> string

let formatInput (input: string list) =
  let newInput = serializeInput input
  Seq.toList newInput |> List.map (fun char -> char.ToString())

let getElementIndex (calculation: string list) (element: string) =
  if List.contains element calculation
  then calculation |> List.findIndexBack (fun x -> x = element)
  else -1

let deleteLastElement (calculation: string list) =
  match calculation.Length with
  | 0 | 1 -> ["0"]
  | _ -> calculation.[..calculation.Length - 2]

let deleteLastPartialCalculation (calculation: string list) =
  let index = getElementIndex calculation "("
  match index with
  | -1 -> calculation
  | _ -> calculation.[..index - 1]

let getLastElement (calculation: string list) =
  match calculation.Length with
  | 0 -> ""
  | _ -> calculation.[calculation.Length - 1]

let appendOperation (calculation: string list) (operation: Operation) =
  List.append calculation [parseOperation operation]

let appendInput (calculation: string list) (input: string list) =
  match getLastElement input with
  | "" -> calculation
  | "." -> List.concat [calculation; deleteLastElement input]
  | _ -> List.append calculation input

let replaceLastOperation (calculation: string list) (operation: Operation) =
  deleteLastElement calculation |> appendOperation <| operation

let countOccurences (calculation: string list) (element: string) =
  let calculationElements = calculation |> List.countBy (fun elem -> elem)
  let matchingElements = calculationElements |> List.filter(fun elem -> fst(elem) = element)
  match matchingElements.Length with
  | 0 -> 0
  | _ -> snd(matchingElements.[0])

let allParenthesesAreClosed (calculation: string list) =
  let numOpenParentheses = countOccurences calculation "("
  match numOpenParentheses with
  | 0 -> false
  | _ -> numOpenParentheses = countOccurences calculation ")"

[<AutoOpen>]
module Calculator

open System
open Fable.Core.JsInterop
open Fable.Import.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.MaterialUI
open Fable.MaterialUI.Core
open Fable.Core

type Activity =
  | Operation of Operation
  | Calculate
  | DigitInput
  | DecimalPointInput
  | OpenParenthesis
  | CloseParenthesis
  | NoActivity

type Model =
  {
    input: string
    calculation: string list
    lastActivity: Activity
    calculationResult: float
  }

type Msg =
  | AppendDigitMsg of string
  | AppendDecimalPointMsg
  | DeleteDigitMsg
  | AddMsg
  | SubtractMsg
  | MultiplyMsg
  | DivideMsg
  | EqualsMsg
  | ClearMsg
  | ClearEntryMsg
  | InvertSignMsg
  | ReplaceInputMsg of string
  | OpenParenthesisMsg
  | CloseParenthesisMsg

let init () =
  {
    input = "0"
    calculation = []
    lastActivity = NoActivity
    calculationResult = 0.0
  }

let appendDigit (model: Model) (digit: string) =
  match model.lastActivity with
  | DecimalPointInput -> {model with input = model.input + digit; lastActivity = DigitInput}
  | DigitInput -> match model.input with
                  | "0" -> {model with input = digit; lastActivity = DigitInput}
                  | _ -> {model with input = model.input + digit; lastActivity = DigitInput}
  | _ -> {model with input = digit; lastActivity = DigitInput}

let appendDecimalPoint (model: Model) =
  match model.lastActivity with
  | NoActivity -> {model with input = model.input + "."; lastActivity = DecimalPointInput}
  | DecimalPointInput -> model
  | DigitInput -> if model.input.Contains "."
                  then model
                  else {model with input = model.input + "."; lastActivity = DecimalPointInput}
  | _ -> {model with input = "0."; lastActivity = DecimalPointInput}

let deleteLastElement (model: Model) =
  match model.input.Length with
  | 1 -> {model with input = "0"; lastActivity = DigitInput}
  | _ -> {model with input = model.input.Substring(0, model.input.Length - 1); lastActivity = DigitInput}

let clearEntry (model: Model) =
  {model with input = "0"; lastActivity = NoActivity}

let invertSign (model: Model) =
  match model.input with
  | "0" -> model
  | _ when model.input.Contains "-" -> {model with input = model.input.Substring(1, model.input.Length - 1); lastActivity = DigitInput}
  | _ -> {model with input = "-" + model.input; lastActivity = DigitInput }

let replaceInput (model: Model) (replacementValue: string) =
  {model with input = replacementValue; lastActivity = DigitInput}

//TODO performOperation - should calculate partial results, taking into account precedence of(i.e. 2 + 3 - shows 5, but 2 + 3 x shows 3 in input)
let performOperation (model: Model) (operation: Operation) =
  match model.lastActivity with
  | DecimalPointInput -> {model with input = (deleteLastElement model).input; calculation = List.append model.calculation [(deleteLastElement model).input; parseOperation operation]; lastActivity = Operation operation}
  | DigitInput | NoActivity -> {model with calculation = List.append model.calculation [model.input; parseOperation operation]; lastActivity = Operation operation}
  | Operation op when operation = op -> model
  | _ -> {model with calculation = List.append model.calculation.[0..(model.calculation.Length - 2)] [parseOperation operation]; lastActivity = Operation operation}

//TODO evaluateCalculation - this doesn't work for longer calculations. It also assumes the calculation is valid
let evaluateCalculation (calculation: string list) =
  match List.length calculation with
  | 0 -> 0.0
  | 1 | 2 -> float calculation.[0]
  | _ ->
    match getOperation calculation.[1] with
    | Add -> float calculation.[0] + float calculation.[2]
    | Subtract -> float calculation.[0] - float calculation.[2]
    | Multiply -> float calculation.[0] * float calculation.[2]
    | Divide -> float calculation.[0] / float calculation.[2]
    | _ -> 0.0

let calculateResult (model: Model) =
  match model.lastActivity with
  | DecimalPointInput -> {model with input = (deleteLastElement model).input; lastActivity = Calculate; calculationResult = float (deleteLastElement model).input }
  | DigitInput ->  { model with input = string (evaluateCalculation (List.append model.calculation [model.input])); lastActivity = Calculate; calculation = []; calculationResult = evaluateCalculation (List.append model.calculation [model.input])}
  | _ -> { model with input = string (evaluateCalculation (List.append model.calculation [model.input])); lastActivity = Calculate; calculation = []; calculationResult = evaluateCalculation (List.append model.calculation [model.input])}

//TODO parentheses...
let update (msg:Msg) (model: Model) =
    match msg with
    | AppendDigitMsg digit -> appendDigit model digit
    | AppendDecimalPointMsg -> appendDecimalPoint model
    | DeleteDigitMsg -> deleteLastElement model
    | AddMsg -> performOperation model Add
    | SubtractMsg -> performOperation model Subtract
    | MultiplyMsg -> performOperation model Multiply
    | DivideMsg -> performOperation model Divide
    | EqualsMsg -> calculateResult model
    | ClearMsg -> init()
    | ClearEntryMsg -> clearEntry model
    | InvertSignMsg -> invertSign model
    | ReplaceInputMsg value -> replaceInput model value
//    | OpenParenthesisMsg -> openParenthesis model
//    | CloseParenthesisMsg -> closeParenthesis model

let digitPressed (key: string) =
  key = "0" || key = "1" || key = "2" || key = "3" || key = "4" || key = "5" || key = "6" || key = "7" || key = "8" || key = "9"
  
let viewDefinition (classes: IClasses) model dispatch =
  div [OnKeyPress (fun keyboardEvent -> if digitPressed keyboardEvent.key then keyboardEvent.key |> AppendDigitMsg |> dispatch)] [
    div [Class classes?display] [
      div [Class classes?calculation] [
        span [] [ model.calculation |> List.fold (+) "" |> str ]
      ]
      div [Class classes?input] [
        span [] [ model.input |> str ]
      ]  
    ]
    div [ Class classes?calculator ] [
      div [ ] [
        div [ Class classes?buttonRow ] [
          button [ Class classes?button; OnClick (fun _ -> ClearMsg |> dispatch) ] [ str "C" ]
          button [ Class classes?button; OnClick (fun _ -> ClearEntryMsg |> dispatch) ] [ str "CE" ]
          button [ Class classes?button; OnClick (fun _ -> dispatch DeleteDigitMsg) ] [
            img [ Src "backspace_grey_24x24.png" ]
          ]
        ]
        div [ Class classes?buttonRow ] [
          button [ Class classes?button; OnClick (fun _ -> AppendDigitMsg "7" |> dispatch) ] [ str "7" ]
          button [ Class classes?button; OnClick (fun _ -> AppendDigitMsg "8" |> dispatch) ] [ str "8" ]
          button [ Class classes?button; OnClick (fun _ -> AppendDigitMsg "9" |> dispatch) ] [ str "9" ]
        ]
        div [ Class classes?buttonRow] [
          button [ Class classes?button; OnClick (fun _ -> AppendDigitMsg "4" |> dispatch) ] [ str "4" ]
          button [ Class classes?button; OnClick (fun _ -> AppendDigitMsg "5" |> dispatch) ] [ str "5" ]
          button [ Class classes?button; OnClick (fun _ -> AppendDigitMsg "6" |> dispatch) ] [ str "6" ]
        ]
        div [ Class classes?buttonRow] [
          button [ Class classes?button; OnClick (fun _ -> AppendDigitMsg "1" |> dispatch) ] [ str "1" ]
          button [ Class classes?button; OnClick (fun _ -> AppendDigitMsg "2" |> dispatch) ] [ str "2" ]
          button [ Class classes?button; OnClick (fun _ -> AppendDigitMsg "3" |> dispatch) ] [ str "3" ]
        ]
        div [ Class classes?buttonRow] [
          button [ Class classes?button; OnClick (fun _ -> dispatch AppendDecimalPointMsg) ] [ str "." ]
          button [ Class classes?button; OnClick (fun _ -> AppendDigitMsg "0" |> dispatch) ] [ str "0" ]
          button [ Class classes?button; OnClick (fun _ -> dispatch InvertSignMsg)] [ str "+ / -" ]
        ]
      ]
      div [ Class classes?operations ] [
        div [ Class classes?operationButtonColumn ] [
          button [ Class (classes?button + " " + (if model.lastActivity = Operation Add then classes?equalsButton else ""))
                   OnClick (fun _ -> dispatch AddMsg) ]
                   [ parseOperation Add |> str ]
          button [ Class (classes?button + " " + (if model.lastActivity = Operation Subtract then classes?equalsButton else ""))
                   OnClick (fun _ -> dispatch SubtractMsg) ]
                   [ parseOperation Subtract |> str ]
          button [ Class (classes?button + " " + (if model.lastActivity = Operation Multiply then classes?equalsButton else ""))
                   OnClick (fun _ -> dispatch MultiplyMsg) ]
                   [ parseOperation Multiply |> str ]
          button [ Class (classes?button + " " + (if model.lastActivity = Operation Divide then classes?equalsButton else ""))
                   OnClick (fun _ -> dispatch DivideMsg) ]
                   [ parseOperation Divide |> str ]
          button [ Class (classes?button + " " + classes?equalsButton)
                   OnClick (fun _ -> dispatch EqualsMsg) ]
                   [ str "=" ]
        ]
        div [ Class classes?operationButtonColumn ] [
          button [ Class classes?button
                   OnClick (fun _ -> (ReplaceInputMsg (string Math.PI) |> dispatch)) ]
                   [ str "π" ]
          button [ Class classes?button
                   OnClick (fun _ -> (ReplaceInputMsg (string Math.E) |> dispatch)) ]
                   [ str "e" ]
          button [ Class classes?button
                   OnClick (fun _ -> (dispatch OpenParenthesisMsg)) ]
                   [ str "(" ]
          button [ Class classes?button
                   OnClick (fun _ -> (dispatch CloseParenthesisMsg)) ]
                   [ str ")" ]
          //TODO Percentage button doesn't work yet
          button [ Class classes?button
                   OnClick (fun _ -> ()) ]
                   [ str "%" ]                  
        ]
      ]
    ]
  ]

let private styles (theme: ITheme) : IStyles list =
  [
    Styles.Custom ("calculator", [
      Display "flex"
    ])
    Styles.Custom ("operations", [
      MarginLeft "25px"
      Display "flex"
    ])
    Styles.Custom ("operationButtonColumn", [
      Display "flex"
      FlexDirection "column"
    ])
    Styles.Custom ("buttonRow", [
      Display "flex"
    ])
    Styles.Button [
      BackgroundColor "rgb(200, 200, 200)"
      MarginTop "10px"
      MarginLeft "10px"
      CSSProp.TextTransform "none"
    ]
    Styles.Custom ("buttonSpace", [
      MarginTop "10px"
      MarginLeft "10px"
      Width "64px"
      Height "36px"
    ])
    Styles.Custom ("display", [
      MarginLeft "10px"
      Border "1px solid rgb(200, 200, 200)"
      BorderTopLeftRadius "5px"
      BorderTopRightRadius "5px"
      BorderBottomLeftRadius "5px"
      BorderBottomRightRadius "5px"
      Width "311px"
      Height "82px"
      PaddingTop "5px"
      PaddingBottom "5px"
      TextAlign "right"
    ])
    Styles.Custom ("calculation", [
      Height "35px"
      Display "flex"
      FlexDirection "column"
      JustifyContent "space-around"
      BorderBottom "1px dashed rgb(200, 200, 200)"
      PaddingLeft "10px"
      PaddingRight "10px"
    ])
    Styles.Custom ("input", [
      Height "35px"
      Display "flex"
      FlexDirection "column"
      JustifyContent "space-around"
      PaddingLeft "10px"
      PaddingRight "10px"
    ])
    Styles.Custom ("equalsButton", [
      BackgroundColor "rgb(90, 190, 60, 0.75)"
      CSSProp.Custom ("&:hover", [
        CSSProp.BackgroundColor "rgb(90, 190, 60, 0.5)"
      ] |> keyValueList CaseRules.LowerFirst)
    ])
  ]
  
type private IProps =
  abstract member model: Model with get, set
  abstract member dispatch: (Msg -> unit) with get, set
  inherit IClassesProps

type private Component(p) =
  inherit PureStatelessComponent<IProps>(p)
  let viewFun (p: IProps) = viewDefinition p.classes p.model p.dispatch
  let viewWithStyles = withStyles (StyleType.Func styles) [] viewFun
  override this.render() = from viewWithStyles this.props []

let view (model: Model) (dispatch: Msg -> unit) : ReactElement =
  let props = jsOptions<IProps>(fun p ->
    p.model <- model
    p.dispatch <- dispatch)
  ofType<Component,_,_> props []

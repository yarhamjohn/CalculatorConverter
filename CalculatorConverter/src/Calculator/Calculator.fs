module Calculator

open System
open Fable.Core.JsInterop
open Fable.Import.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.MaterialUI
open Fable.MaterialUI.Core
open Fable.Core

type Model =
  {
    input: string list
    lastActivity: Activity
    currentOperation: Operation
    calculation: string list
  }

type Msg =
  | AppendDigitMsg of string
  | AppendDecimalPointMsg
  | DeleteDigitMsg
  | AddMsg
  | SubstractMsg
  | MultiplyMsg
  | DivideMsg
  | EqualsMsg
  | ClearMsg
  | ClearEntryMsg
  | InvertSignMsg
  | ReplaceInputMsg of string list
  | OpenParenthesisMsg
  | CloseParenthesisMsg

let init () =
  {
    input = ["0"]
    lastActivity = NoActivity
    currentOperation = NoOperation
    calculation = []
  }

let inputDigit (model: Model) (digit: string) =
  let newInput = appendDigit model.input digit |> formatInput
  { model with input = newInput; lastActivity = DigitInput }

let inputDecimalPoint (model: Model) =
  let newInput = appendDecimalPoint model.input |> formatInput
  { model with input = newInput; lastActivity = DecimalPointInput }

let openParenthesis (model: Model) =
  let newCalculation =
    if model.calculation.Length > 0 && getLastElement model.calculation = ")"
    then deleteLastPartialCalculation model.calculation
    else List.append model.calculation ["("]

  { model with lastActivity = OpenParenthesis; calculation = newCalculation }

//TODO: Need to calculate the appropriate input...
let closeParenthesis (model: Model) =
  if allParenthesesAreClosed model.calculation
  then model
  else { model with lastActivity = CloseParenthesis; calculation = List.concat [model.calculation; model.input; [")"; " "]] }

let performOperation (model: Model) (operation: Operation) =
  let newCalculation =
    match model.lastActivity with
    | Operation -> replaceLastOperation model.calculation operation
    | CloseParenthesis -> appendOperation model.calculation operation
    | _ -> appendInput model.calculation model.input
           |> appendOperation <| operation

  let newInput = if model.lastActivity = Operation
                 then model.input
                 else calculate model.calculation

  {model with
    input = newInput;
    lastActivity = Operation;
    currentOperation = operation;
    calculation = newCalculation}

let calculateResult (model: Model) =
  let finalCalculation = List.append model.calculation [serializeInput model.input]
  {model with
    input = calculate finalCalculation |> formatInput;
    lastActivity = Calculate;
    currentOperation = NoOperation;
    calculation = []}

let update (msg:Msg) (model: Model) =
    match msg with
    | AppendDigitMsg digit -> inputDigit model digit
    | AppendDecimalPointMsg -> inputDecimalPoint model
    | DeleteDigitMsg -> { model with input = deleteLastElement model.input }
    | AddMsg -> performOperation model Add
    | SubstractMsg -> performOperation model Subtract
    | MultiplyMsg -> performOperation model Multiply
    | DivideMsg -> performOperation model Divide
    | EqualsMsg -> calculateResult model
    | ClearMsg -> init()
    | ClearEntryMsg -> { model with input = ["0"] }
    | InvertSignMsg -> { model with input = [string ((evaluateInput model.input) * -1.0)] }
    | ReplaceInputMsg value -> { model with input = value }
    | OpenParenthesisMsg -> openParenthesis model
    | CloseParenthesisMsg -> closeParenthesis model

let digitPressed (key: string) =
  key = "0" || key = "1" || key = "2" || key = "3" || key = "4" || key = "5" || key = "6" || key = "7" || key = "8" || key = "9"
  
let viewDefinition (classes: IClasses) model dispatch =
  div [OnKeyPress (fun keyboardEvent -> if digitPressed keyboardEvent.key then keyboardEvent.key |> AppendDigitMsg |> dispatch)] [
    div [Class classes?display] [
      div [Class classes?calculation] [
        span [] [ str (model.calculation |> List.fold (+) "") ]
      ]
      div [Class classes?input] [
        span [] [ serializeInput model.input |> str ]
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
          button [ Class (classes?button + " " + (if model.currentOperation = Add then classes?equalsButton else ""))
                   OnClick (fun _ -> dispatch AddMsg) ]
                   [ parseOperation Add |> str ]
          button [ Class (classes?button + " " + (if model.currentOperation = Subtract then classes?equalsButton else ""))
                   OnClick (fun _ -> dispatch SubstractMsg) ]
                   [ parseOperation Subtract |> str ]
          button [ Class (classes?button + " " + (if model.currentOperation = Multiply then classes?equalsButton else ""))
                   OnClick (fun _ -> dispatch MultiplyMsg) ]
                   [ parseOperation Multiply |> str ]
          button [ Class (classes?button + " " + (if model.currentOperation = Divide then classes?equalsButton else ""))
                   OnClick (fun _ -> dispatch DivideMsg) ]
                   [ parseOperation Divide |> str ]
          button [ Class (classes?button + " " + classes?equalsButton)
                   OnClick (fun _ -> dispatch EqualsMsg) ]
                   [ str "=" ]
        ]
        div [ Class classes?operationButtonColumn ] [
          button [ Class classes?button
                   OnClick (fun _ -> (ReplaceInputMsg ([string Math.PI]) |> dispatch)) ]
                   [ str "π" ]
          button [ Class classes?button
                   OnClick (fun _ -> (ReplaceInputMsg ([string Math.E]) |> dispatch)) ]
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

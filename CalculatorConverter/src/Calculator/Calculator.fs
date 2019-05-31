module Calculator

open Fable.Core.JsInterop
open Fable.Import.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.MaterialUI
open Fable.MaterialUI.Core
open Fable.Core

type Model =
  {
    input: string
    stored: string
    lastActivity: Activity
    operation: Operation
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

let init () =
  let model =
    {
      input = "0"
      stored = "0"
      lastActivity = NoActivity
      operation = NoOperation
      calculation = []
    }
  model

let inputDigit (model: Model) (digit: string) =
  let newInput =
    if model.lastActivity = DigitInput || model.lastActivity = DecimalPointInput
    then appendDigitToInput model.input digit
    else digit

  {model with input = newInput; lastActivity = DigitInput}

let inputDecimalPoint (model: Model) =
  let newInput =
    if model.lastActivity = DigitInput
    then appendDecimalPointToInput model.input
    else "0."

  {model with input = newInput; lastActivity = DecimalPointInput}

let performOperation (model: Model) (operation: Operation) =
  let operationSymbol = parseOperation operation
  let newCalculation =
    if model.lastActivity = Operation
    then
      let shortenedList = model.calculation.[..model.calculation.Length - 3] //slicing is inclusive but index is 0-based
      List.append shortenedList [operationSymbol; " "]
    else
      let valueToDisplay = if model.lastActivity = DecimalPointInput then deleteFromInput model.input else model.input
      List.append model.calculation [valueToDisplay; " "; operationSymbol; " "]

  let result = if model.lastActivity = Operation then model.input else calculate model.stored model.input model.operation

  {model with
    input = result;
    stored = result;
    lastActivity = Operation;
    operation = operation;
    calculation = newCalculation}

let calculateResult (model: Model) =
  {model with
    input = calculate model.stored model.input model.operation;
    stored = "0";
    lastActivity = Calculate;
    operation = NoOperation;
    calculation = []}

let update (msg:Msg) (model: Model) =
    match msg with
    | AppendDigitMsg digit -> inputDigit model digit
    | AppendDecimalPointMsg -> inputDecimalPoint model
    | DeleteDigitMsg -> { model with input = deleteFromInput model.input }
    | AddMsg -> performOperation model Add
    | SubstractMsg -> performOperation model Subtract
    | MultiplyMsg -> performOperation model Multiply
    | DivideMsg -> performOperation model Divide
    | EqualsMsg -> calculateResult model
    | ClearMsg -> init()
    | ClearEntryMsg -> { model with input = "0" }
    | InvertSignMsg -> { model with input = string (float model.input * -1.0) }

let digitPressed (key: string) =
  key = "0" || key = "1" || key = "2" || key = "3" || key = "4" || key = "5" || key = "6" || key = "7" || key = "8" || key = "9"
  
let viewDefinition (classes: IClasses) model dispatch =
  div [OnKeyPress (fun keyboardEvent -> if digitPressed keyboardEvent.key then keyboardEvent.key |> AppendDigitMsg |> dispatch)] [
    div [Class classes?display] [
      div [Class classes?calculation] [
        span [] [ str (model.calculation |> List.fold (+) "") ]
      ]
      div [Class classes?input] [
        span [] [ str model.input ]
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
          button [ Class (classes?button + " " + (if model.operation = Add then classes?equalsButton else ""))
                   OnClick (fun _ -> dispatch AddMsg) ]
                   [ parseOperation Add |> str ]
          button [ Class (classes?button + " " + (if model.operation = Subtract then classes?equalsButton else ""))
                   OnClick (fun _ -> dispatch SubstractMsg) ]
                   [ parseOperation Subtract |> str ]
          button [ Class (classes?button + " " + (if model.operation = Multiply then classes?equalsButton else ""))
                   OnClick (fun _ -> dispatch MultiplyMsg) ]
                   [ parseOperation Multiply |> str ]
          button [ Class (classes?button + " " + (if model.operation = Divide then classes?equalsButton else ""))
                   OnClick (fun _ -> dispatch DivideMsg) ]
                   [ parseOperation Divide |> str ]
          button [ Class (classes?button + " " + classes?equalsButton)
                   OnClick (fun _ -> dispatch EqualsMsg) ]
                   [ str "=" ]
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

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
    action: string
    calculation: string list
  }

type Msg =
  | AppendDigit of string
  | AppendDecimalPoint
  | DeleteDigit
  | Add
  | Substract
  | Multiply
  | Divide
  | Equals
  | Clear

let init () =
  let model =
    {
      input = "0"
      stored = "0"
      action = ""
      calculation = []
    }
  model
  
let updateAction (model: Model) (action: string) =
  let display = if model.action = "=" then [" "; action; " "] else [model.input; " "; action; " "]
  let nextInput = if model.action = "" then "0" else calculate model.action model.stored model.input
  let m =
    {
      input = nextInput
      stored = model.input
      action = action
      calculation = List.append model.calculation display
    }
  m
  
let updateResult (model: Model) =
  let result = calculate model.action model.stored model.input
  let m =
    {
      input = result
      stored = "0"
      action = "="
      calculation = List.append model.calculation [model.input]
    }
  m

let update (msg:Msg) (model: Model) =
    match msg with
    | AppendDigit digit -> { model with input = appendDigitToInput model.input digit }
    | AppendDecimalPoint -> { model with input = appendDecimalPointToInput model.input }
    | DeleteDigit -> { model with input = deleteFromInput model.input }
    | Add -> updateAction model "+"  
    | Substract -> updateAction model "-"  
    | Multiply -> updateAction model "*"  
    | Divide -> updateAction model "/"  
    | Equals -> updateResult model
    | Clear -> init()

let viewDefinition (classes: IClasses) model dispatch =
  div [] [
    div [Class classes?calculation] [
      span [] [ str (model.calculation |> List.fold (+) "") ]
    ]
    div [ Class classes?calculator ] [
      div [ Class classes?digits ] [
        div [ Class classes?buttonRow ] [
          button [ Class classes?button; OnClick (fun _ -> Clear |> dispatch) ] [ str "C" ]
          button [ Class classes?button; ] [ str "" ]
          button [ Class classes?button; ] [ str "" ]
        ]
        div [ Class classes?buttonRow ] [
          button [ Class classes?button; OnClick (fun _ -> AppendDigit "7" |> dispatch) ] [ str "7" ]
          button [ Class classes?button; OnClick (fun _ -> AppendDigit "8" |> dispatch) ] [ str "8" ]
          button [ Class classes?button; OnClick (fun _ -> AppendDigit "9" |> dispatch) ] [ str "9" ]
        ]
        div [ Class classes?buttonRow] [
          button [ Class classes?button; OnClick (fun _ -> AppendDigit "4" |> dispatch) ] [ str "4" ]
          button [ Class classes?button; OnClick (fun _ -> AppendDigit "5" |> dispatch) ] [ str "5" ]
          button [ Class classes?button; OnClick (fun _ -> AppendDigit "6" |> dispatch) ] [ str "6" ]
        ]
        div [ Class classes?buttonRow] [
          button [ Class classes?button; OnClick (fun _ -> AppendDigit "1" |> dispatch) ] [ str "1" ]
          button [ Class classes?button; OnClick (fun _ -> AppendDigit "2" |> dispatch) ] [ str "2" ]
          button [ Class classes?button; OnClick (fun _ -> AppendDigit "3" |> dispatch) ] [ str "3" ]
        ]
        div [ Class classes?buttonRow] [
          button [ Class classes?button; OnClick (fun _ -> AppendDigit "0" |> dispatch) ] [ str "0" ]
          button [ Class classes?button; OnClick (fun _ -> dispatch AppendDecimalPoint) ] [ str "." ]
          button [ Class classes?button; OnClick (fun _ -> dispatch DeleteDigit) ] [
          img [ Src "backspace_grey_24x24.png" ]
          ]
        ]
        div [ Class classes?result ] [
          span [Class classes?display] [ str model.input ]
        ]
      ]
      div [ Class classes?actions ] [
        div [ Class classes?actionButtonColumn ] [
          button [ Class classes?button; OnClick (fun _ -> dispatch Add) ] [ str "+" ]
          button [ Class classes?button; OnClick (fun _ -> dispatch Substract) ] [ str "-" ]
          button [ Class classes?button; OnClick (fun _ -> dispatch Multiply) ] [ str "*" ]
          button [ Class classes?button; OnClick (fun _ -> dispatch Divide) ] [ str "/" ]
          button [ classList [!!classes?button, true; !!classes?equalsButton, true]; OnClick (fun _ -> dispatch Equals) ] [ str "=" ]
        ]
      ]
    ]
  ]

let private styles (theme: ITheme) : IStyles list =
  [
    Styles.Custom ("calculator", [
      Display "flex"
    ])
    Styles.Custom ("digits", [
    ])
    Styles.Custom ("actions", [
      MarginLeft "25px"
    ])
    Styles.Custom ("actionButtonColumn", [
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
    Styles.Custom ("calculation", [
      MarginTop "10px"
      MarginLeft "10px"
      Border "1px dashed rgb(200, 200, 200)"
      BorderTopLeftRadius "5px"
      BorderTopRightRadius "5px"
      BorderBottomLeftRadius "5px"
      BorderBottomRightRadius "5px"
      Width "212px"
      Height "36px"
      PaddingLeft "10px"
      PaddingRight "10px"
      PaddingTop "5px"
      PaddingBottom "5px"
      Display "flex"
      FlexDirection "column"
      JustifyContent "space-around"
    ])
    Styles.Custom ("result", [
      BackgroundColor "white"
      MarginTop "10px"
      MarginLeft "10px"
      Border "1px solid rgb(200, 200, 200)"
      BorderTopLeftRadius "5px"
      BorderTopRightRadius "5px"
      BorderBottomLeftRadius "5px"
      BorderBottomRightRadius "5px"
      Width "212px"
      Height "36px"
      PaddingLeft "10px"
      PaddingRight "10px"
      PaddingTop "5px"
      PaddingBottom "5px"
      Display "flex"
      FlexDirection "column"
      JustifyContent "space-around"
    ])
    Styles.Custom ("display", [
      TextAlign "right"
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

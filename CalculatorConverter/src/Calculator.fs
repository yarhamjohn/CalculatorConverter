module Calculator

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Elmish architecture and samples at https://elmish.github.io/
*)

open Fable.Core.JsInterop
open Fable.Import.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.MaterialUI
open Fable.MaterialUI.Core

type Model =
  {
    number: int
  }

type Msg =
  | Increment
  | Decrement

let init () =
  let model =
    {
      number = 0
    }
  model

let update (msg:Msg) (model:Model) =
    match msg with
    | Increment -> { model with number = model.number + 1 }
    | Decrement -> { model with number = model.number - 1 }

let viewDefinition (classes: IClasses) model dispatch =
  div [] [
    div [Class classes?calculator] [
      div [Class classes?buttonRow] [
        button [
          Class classes?button
          OnClick (fun _ -> dispatch Increment)
        ] [ str "7" ]
        button [
          Class classes?button
          OnClick (fun _ -> dispatch Increment)
        ] [ str "8" ]
        button [
          Class classes?button
          OnClick (fun _ -> dispatch Increment)
        ] [ str "9" ]
      ]
      div [Class classes?buttonRow] [
        button [
          Class classes?button
          OnClick (fun _ -> dispatch Increment)
        ] [ str "4" ]
        button [
          Class classes?button
          OnClick (fun _ -> dispatch Increment)
        ] [ str "5" ]
        button [
          Class classes?button
          OnClick (fun _ -> dispatch Increment)
        ] [ str "6" ]
      ]
      div [Class classes?buttonRow] [
        button [
          Class classes?button
          OnClick (fun _ -> dispatch Increment)
        ] [ str "1" ]
        button [
          Class classes?button
          OnClick (fun _ -> dispatch Increment)
        ] [ str "2" ]
        button [
          Class classes?button
          OnClick (fun _ -> dispatch Increment)
        ] [ str "3" ]
      ]
    ]
    div [] [ ofInt model.number ]
  ]


  
let private styles (theme: ITheme) : IStyles list =
  [
    Styles.Custom ("calculator", [
      Display "flex"
      FlexDirection "column"
      Height "120px"
      JustifyContent "space-between"
    ])
    Styles.Custom ("buttonRow", [
      Display "flex"
      JustifyContent "space-between"
      Width "200px"
    ])
    Styles.Button [
      BackgroundColor "rgb(200, 200, 200)"
    ]
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

module Calculator

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Elmish architecture and samples at https://elmish.github.io/
*)

open System
open Fable.Core.JsInterop
open Fable.Import.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.MaterialUI
open Fable.MaterialUI.Core

type Model =
  {
    input: string
  }

type Msg =
  | Combine of string
  | Increment
  | Decrement

let init () =
  let model =
    {
      input = "0"
    }
  model

let update (msg:Msg) (model:Model) =
    match msg with
    | Combine c -> { model with input = model.input + c}

let viewDefinition (classes: IClasses) model dispatch =
  div [] [
    div [Class classes?calculator] [
      div [Class classes?buttonRow] [
        button [
          Class classes?button
          OnClick (fun _ -> Combine "7" |> dispatch)
        ] [ str "7" ]
        button [
          Class classes?button
          OnClick (fun _ -> Combine "8" |> dispatch)
        ] [ str "8" ]
        button [
          Class classes?button
          OnClick (fun _ -> Combine "9" |> dispatch)
        ] [ str "9" ]
      ]
      div [Class classes?buttonRow] [
        button [
          Class classes?button
          OnClick (fun _ -> Combine "4" |> dispatch)
        ] [ str "4" ]
        button [
          Class classes?button
          OnClick (fun _ -> Combine "5" |> dispatch)
        ] [ str "5" ]
        button [
          Class classes?button
          OnClick (fun _ -> Combine "6" |> dispatch)
        ] [ str "6" ]
      ]
      div [Class classes?buttonRow] [
        button [
          Class classes?button
          OnClick (fun _ -> Combine "1" |> dispatch)
        ] [ str "1" ]
        button [
          Class classes?button
          OnClick (fun _ -> Combine "2" |> dispatch)
        ] [ str "2" ]
        button [
          Class classes?button
          OnClick (fun _ -> Combine "3" |> dispatch)
        ] [ str "3" ]
      ]
    ]
    div [Class classes?result] [ model.input |> Int32.Parse |> ofInt ]
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
    Styles.Custom ("result", [
      BackgroundColor "white"
      MarginTop "25px"
      Border "1px solid black"
      BorderTopLeftRadius "5px"
      BorderTopRightRadius "5px"
      BorderBottomLeftRadius "5px"
      BorderBottomRightRadius "5px"
      Width "200px"
      PaddingLeft "10px"
      PaddingRight "10px"
      PaddingTop "5px"
      PaddingBottom "5px"
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

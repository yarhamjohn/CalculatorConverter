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

type Model = int

type Msg =
  | Increment
  | Decrement

let init() : Model = 0

let update (msg:Msg) (model:Model) =
    match msg with
    | Increment -> model + 1
    | Decrement -> model - 1

let view' (model:Model) dispatch =
  div []
      [
        button [ OnClick (fun _ -> dispatch Increment) ] [ str "+" ]
        div [] [ str (string model) ]
        button [ OnClick (fun _ -> dispatch Decrement) ] [ str "-" ]
      ]

let private styles (theme: ITheme) : IStyles list =
  let drawerWidth = "240px"
  [
    Styles.Root [
      Display "flex"
    ]
    Styles.Custom ("appBar", [
      CSSProp.ZIndex (theme.zIndex.drawer + 1)
    ])
    Styles.Custom ("drawer", [
      Width drawerWidth
      FlexShrink 0
    ])
    Styles.Custom ("drawerPaper", [
      Width drawerWidth
    ])
    Styles.Custom ("content", [
      FlexGrow 1
      CSSProp.Padding (theme.spacing.unit * 3)
    ])
    Styles.Custom' ("toolbar", theme.mixins.toolbar)
  ]
  
type private IProps =
  abstract member model: Model with get, set
  abstract member dispatch: (Msg -> unit) with get, set
  inherit IClassesProps

type private Component(p) =
  inherit PureStatelessComponent<IProps>(p)
  let viewFun (p: IProps) = view' p.model p.dispatch
  let viewWithStyles = withStyles (StyleType.Func styles) [] viewFun
  override this.render() = from viewWithStyles this.props []

let view (model: Model) (dispatch: Msg -> unit) : ReactElement =
  let props = jsOptions<IProps>(fun p ->
    p.model <- model
    p.dispatch <- dispatch)
  ofType<Component,_,_> props []

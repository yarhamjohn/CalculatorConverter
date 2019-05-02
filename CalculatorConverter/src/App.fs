module App

open Elmish
open Elmish.React
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import.React
open Fable.MaterialUI
open Fable.MaterialUI.Core

type Page =
    | Calculator
    | Converter
    static member All = [ Calculator; Converter ]
    
type Model =
    {
        Page: Page
        Calculator: Calculator.Model
        Converter: Converter.Model
    }

type Msg =
    | Navigate of Page
    | CalculatorMsg of Calculator.Msg
    | ConverterMsg of Converter.Msg


let init () =
  let model =
    {
      Page = Calculator
      Calculator = Calculator.init()
      Converter = Converter.init()
    }
  model, Cmd.none

let update (msg:Msg) (model:Model) =
    match msg with
    | Navigate p -> { model with Page = p }, Cmd.none
    | CalculatorMsg calcMsg -> { model with Calculator = Calculator.update calcMsg model.Calculator }, Cmd.none
    | ConverterMsg convMsg -> { model with Converter = Converter.update convMsg model.Converter }, Cmd.none

let pageTitle = function
    | Calculator -> "Calculator"
    | Converter -> "Converter"

let private pageListItem model dispatch page =
  listItem [
    ListItemProp.Button true
    HTMLAttr.Selected (model.Page = page)
    Key (pageTitle page)
    DOMAttr.OnClick (fun _ -> Navigate page |> dispatch)
  ] [
    listItemText [ ] [ page |> pageTitle |> str ]
  ]
  
let private pageView model dispatch =
  match model.Page with
  | Calculator -> lazyView2 Calculator.view model.Calculator (CalculatorMsg >> dispatch)
  | Converter -> lazyView2 Converter.view model.Converter (ConverterMsg >> dispatch)
  
let private viewDefinition (classes: IClasses) model dispatch =
  div [
    Class classes?root
  ] [
    cssBaseline []
    appBar [
      Class classes?appBar
      AppBarProp.Position AppBarPosition.Fixed
    ] [
      toolbar [] [
        typography [
          TypographyProp.Variant TypographyVariant.H6
          MaterialProp.Color ComponentColor.Inherit
        ] [ "Calculator and Converter built with F# and Elm" |> str ]
      ]
    ]
    drawer [
      Class classes?drawer
      DrawerProp.Variant DrawerVariant.Permanent
      Classes [ ClassNames.Paper classes?drawerPaper ]
    ] [
      div [ Class classes?toolbar ] []
      list [ Component !^"nav" ] [
        Page.All |> List.map (pageListItem model dispatch) |> ofList
      ]
    ]
    main [ Class classes?content ] [
      div [ Class classes?toolbar ] []
      pageView model dispatch
    ]
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
  let viewFun (p: IProps) = viewDefinition p.classes p.model p.dispatch
  let viewWithStyles = withStyles (StyleType.Func styles) [] viewFun
  override this.render() = from viewWithStyles this.props []

let view (model: Model) (dispatch: Msg -> unit) : ReactElement =
  let props = jsOptions<IProps>(fun p ->
    p.model <- model
    p.dispatch <- dispatch)
  ofType<Component,_,_> props []
  
Program.mkProgram init update view
|> Program.withReact "elmish-app"
|> Program.withConsoleTrace
|> Program.run

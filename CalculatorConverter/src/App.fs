module App

open Elmish
open Elmish.React
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
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
  
let view (model:Model) dispatch =
  div [] [
    div [] [
      list [ Component !^"nav" ] [
        Page.All |> List.map (pageListItem model dispatch) |> ofList
      ]
    ]
    main [] [
      div [] [
         pageView model dispatch
    ]
  ]
]

Program.mkProgram init update view
|> Program.withReact "elmish-app"
|> Program.withConsoleTrace
|> Program.run

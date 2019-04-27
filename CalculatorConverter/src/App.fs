module App

open Elmish
open Elmish.React
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.MaterialUI
open Fable.MaterialUI.Core

type Page =
    | Home
    | Counter
    static member All = [ Home; Counter ]
    
type Model =
    {
        Page: Page
        Counter: Counter.Model
    }

type Msg =
    | Navigate of Page
    | CounterMsg of Counter.Msg


let init () =
  let m =
    {
      Page = Home
      Counter = Counter.init()
    }
  m, Cmd.none

let update (msg:Msg) (model:Model) =
    match msg with
    | Navigate p -> { model with Page = p }, Cmd.none
    | CounterMsg m -> { model with Counter = Counter.update m model.Counter }, Cmd.none

let pageTitle = function
    | Home -> "Home"
    | Counter -> "Counter"

let private pageListItem model dispatch page =
  listItem [
    ListItemProp.Button true
    ListItemProp.Divider (page = Home)
    HTMLAttr.Selected (model.Page = page)
    Key (pageTitle page)
    DOMAttr.OnClick (fun _ -> Navigate page |> dispatch)
  ] [
    listItemText [ ] [ page |> pageTitle |> str ]
  ]
  
let private pageView model dispatch =
  match model.Page with
  | Home -> typography [] [ str "This app is a calculator and unit converter." ]
  | Counter -> lazyView2 Counter.view model.Counter (CounterMsg >> dispatch)
  
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

// App
Program.mkProgram init update view
|> Program.withReact "elmish-app"
|> Program.withConsoleTrace
|> Program.run

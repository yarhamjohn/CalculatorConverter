module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Elmish architecture and samples at https://elmish.github.io/
*)

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
    
// Set up page model and message
type Model =
    {
        Page: Page
        Counter: Counter.Model
    }

type Msg =
    | Navigate of Page
    | CounterMsg of Counter.Msg

// create initialise and update methods
let init () =
  let m =
    { Page = Home
      Counter = Counter.init() }
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
  
// create the react view
let view (model:Model) dispatch =

  div [] [
      list [ Component !^"nav" ] [
        Page.All |> List.map (pageListItem model dispatch) |> ofList
      ]
    ]

// App
Program.mkProgram init update view
|> Program.withReact "elmish-app"
|> Program.withConsoleTrace
|> Program.run

module Main (..) where

import Model exposing (Model)
import Layout exposing (Layout)
import Http
import Task exposing (Task)
import Layout.StartApp
import Effects exposing (Effects, Never)
import Dict exposing (Dict)
import Amount exposing (Amount)


type State
  = InitialLoad
  | Active Model


type Action
  = Init
  | Load (Result Http.Error String)
  | Debug String String Int


placeholderList : List a -> Layout
placeholderList list =
  list
    |> List.map Layout.placeholder
    |> Layout.list 50


view : Signal.Address Action -> State -> Layout
view address m =
  let
    account a =
      Layout.placeholder a.balance
        |> Layout.left 150 (Layout.placeholder a.name)

    debugAction title action =
      Layout.placeholder title
        |> Layout.onClick (Signal.message address action)

    debugActions =
      Layout.flow
        ( 100, 50 )
        [ debugAction "Income $100" (Debug "income" "checking" 100)
        , debugAction "Spend $12" (Debug "checking" "expenses" 12)
        , debugAction "Spend $25" (Debug "checking" "expenses" 25)
        ]
  in
    case m of
      InitialLoad ->
        Layout.placeholder m

      Active model ->
        Dict.values model.accounts
          |> List.map account
          |> Layout.list 50
          |> Layout.bottom 150 debugActions


update : Action -> State -> ( State, Effects Action )
update action m =
  case ( action, m ) of
    ( Init, model ) ->
      ( model, Effects.none )

    ( Load _, InitialLoad ) ->
      ( Active Model.init, Effects.none )

    ( Load _, _ ) ->
      ( m, Effects.none )

    ( Debug from to dollars, Active model ) ->
      case
        model
          |> Model.update (Model.Transaction (Amount (dollars * 100) Amount.USD_Cents) (Model.AccountId from) (Model.AccountId to))
      of
        Ok model' ->
          ( Active model', Effects.none )

        Err message ->
          ( m, Effects.none )

    ( Debug _ _ _, _ ) ->
      ( m, Effects.none )


actions : Signal.Mailbox Action
actions =
  Signal.mailbox Init


app =
  Layout.StartApp.start
    { init = ( InitialLoad, Effects.task <| Task.map Load <| Task.toResult <| Http.getString "/" )
    , inputs = []
    , update = update
    , view = view
    }


port runner : Signal (Task Never ())
port runner =
  app.tasks


main =
  app.html

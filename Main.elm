module Main (..) where

import Model exposing (Model)
import Layout exposing (Layout)
import Http
import Task exposing (Task)
import Layout.StartApp
import Effects exposing (Effects, Never)


type State
  = InitialLoad
  | Active Model


type Action
  = Init
  | Load (Result Http.Error String)


view : Signal.Address Action -> State -> Layout
view _ model =
  Layout.placeholder model


update : Action -> State -> ( State, Effects Action )
update action m =
  case action of
    Load _ ->
      ( Active Model.init, Effects.none )

    Init ->
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

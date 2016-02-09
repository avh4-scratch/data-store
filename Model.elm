module Model (..) where

import Dict exposing (Dict)
import Amount exposing (Amount, Currency(..))
import Account exposing (Account)


type AccountId
  = AccountId String


type alias Transaction =
  { amount : Amount
  , source : AccountId
  , target : AccountId
  }


type alias Model =
  { accounts : Dict String Account }


accountBalance : AccountId -> Model -> Maybe Amount
accountBalance (AccountId accountId) { accounts } =
  Dict.get accountId accounts
    |> Maybe.map .balance


init : Model
init =
  { accounts =
      Dict.empty
        |> Dict.insert "checking" (Account (Amount 0 USD_Cents) "Checking")
        |> Dict.insert "expenses" (Account (Amount 0 USD_Cents) "Expenses")
  }


update : Transaction -> Model -> Result String Model
update tx model =
  let
    (AccountId sourceId) =
      tx.source

    (AccountId targetId) =
      tx.target

    newSource : Result String Account
    newSource =
      Dict.get sourceId model.accounts
        |> Result.fromMaybe "source account doesn't exist"
        |> (flip Result.andThen) (Account.subtract tx.amount)

    newTarget =
      Dict.get targetId model.accounts
        |> Result.fromMaybe "target account doesn't exist"
        |> (flip Result.andThen) (Account.add tx.amount)
  in
    case ( newSource, newTarget ) of
      ( Ok newSource', Ok newTarget' ) ->
        { model
          | accounts =
              model.accounts
                |> Dict.insert sourceId newSource'
                |> Dict.insert targetId newTarget'
        }
          |> Ok

      ( Err message, _ ) ->
        Err message

      ( _, Err message ) ->
        Err message

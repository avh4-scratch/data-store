module Account (..) where

import Amount exposing (Amount)


type alias Account =
  { balance : Amount
  , name : String
  }


add : Amount -> Account -> Result String Account
add amount account =
  Amount.add amount account.balance
    |> Result.map (\amount' -> { account | balance = amount' })


subtract : Amount -> Account -> Result String Account
subtract amount account =
  Amount.subtract amount account.balance
    |> Result.map (\amount' -> { account | balance = amount' })

module Tests (..) where

import ElmTest exposing (..)
import Amount exposing (..)
import Model exposing (..)


all : Test
all =
  suite
    "A Test Suite"
    [ init
        |> accountBalance (AccountId "checking")
        |> assertEqual (Just <| Amount 0 USD_Cents)
        |> test "empty account"
    , init
        |> accountBalance (AccountId "zxcbvnm")
        |> assertEqual (Nothing)
        |> test "non-existent account"
    , init
        |> update (Transaction (Amount 100 USD_Cents) (AccountId "checking") (AccountId "expenses"))
        |> Result.map (accountBalance (AccountId "checking"))
        |> assertEqual (Ok <| Just <| Amount -100 USD_Cents)
        |> test "transaction deducts from source"
    , init
        |> update (Transaction (Amount 100 USD_Cents) (AccountId "checking") (AccountId "expenses"))
        |> Result.map (accountBalance (AccountId "expenses"))
        |> assertEqual (Ok <| Just <| Amount 100 USD_Cents)
        |> test "transaction adds to target"
    ]

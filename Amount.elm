module Amount (Amount, Currency(..), add, subtract) where


type Currency
  = USD_Cents


type alias Amount =
  { value : Int
  , currency : Currency
  }


add : Amount -> Amount -> Result String Amount
add a b =
  if a.currency == b.currency then
    Ok
      <| { value = a.value + b.value
         , currency = a.currency
         }
  else
    Err "currencies don't match"


subtract : Amount -> Amount -> Result String Amount
subtract a b =
  add { a | value = -a.value } b

module UInt64.Internal exposing
    ( Base(..)
    , Digits(..)
    , UpperOrLower(..)
    )


type UpperOrLower
    = Upper
    | Lower


type Base
    = Decimal
    | Hex UpperOrLower
    | Octal
    | Binary


{-|

  - `Int` is list length

-}
type Digits a
    = Digits ( Int, List a )

module TestUtil exposing
    ( anyChar
    , largeInt
    , riskyIntToCharDigit
    , smallInt
    , uint52
    , uint53
    , uint64
    , uintUniformByBitsize
    , uintUniformByValue
    )

import Fuzz exposing (Fuzzer)
import UInt64 exposing (UInt64)



-- CONSTANTS


limit30 : Int
limit30 =
    0x40000000



-- FUZZERS


anyChar : Fuzzer Char
anyChar =
    -- NOTE: Don't generate lone high/low surrogates as this seems to cause problems with `elm-test`.
    --       Such invalid String:s are possible in Elm, and so I'd like to test them,
    --       but that doesn't seem possible for now.
    Fuzz.frequency
        [ ( 3, Fuzz.map Char.fromCode <| Fuzz.intRange 0 0xD7FF )
        , ( 1, Fuzz.map Char.fromCode <| Fuzz.intRange 0xE000 0xFFFF )
        , ( 3, Fuzz.map Char.fromCode <| Fuzz.intRange 0x00100000 0x0010FFFF )
        ]


{-| For arguments checked with `limitLargeInt`.
-}
largeInt : Fuzzer Int
largeInt =
    Fuzz.frequency
        [ -- below safe integer range
          ( 1, Fuzz.constant -9007199254740992 )

        -- negative values aren't that interesting here
        , ( 1, Fuzz.map negate uint53NonZero )
        , ( 1, Fuzz.constant 0 )
        , ( 10, uint53NonZero )

        -- above safe integer range
        , ( 1, Fuzz.constant 9007199254740992 )
        ]


{-| For arguments checked with `limitSmallInt`.
-}
smallInt : Fuzzer Int
smallInt =
    Fuzz.frequency
        [ -- below safe integer range
          ( 1, Fuzz.constant -9007199254740992 )
        , ( 5, Fuzz.map negate uint53NonZero )
        , ( 1, Fuzz.constant 0 )
        , ( 5, uint53NonZero )

        -- above safe integer range
        , ( 1, Fuzz.constant 9007199254740992 )
        ]


uint52 : Fuzzer Int
uint52 =
    Fuzz.frequency
        [ ( 1, Fuzz.constant 0 )
        , ( 10, uint52NonZero )
        ]


uint52NonZero : Fuzzer Int
uint52NonZero =
    Fuzz.frequency
        [ ( 30, uintUniformByBitsize 1 30 )
        , ( 22, Fuzz.map2 (\a b -> a * limit30 + b) (uintUniformByBitsize 1 22) (uintUniformByValue 30) )
        ]


uint53 : Fuzzer Int
uint53 =
    Fuzz.frequency
        [ ( 1, Fuzz.constant 0 )
        , ( 10, uint53NonZero )
        ]


uint53NonZero : Fuzzer Int
uint53NonZero =
    Fuzz.frequency
        [ ( 30, uintUniformByBitsize 1 30 )
        , ( 23, Fuzz.map2 (\a b -> a * limit30 + b) (uintUniformByBitsize 1 23) (uintUniformByValue 30) )
        ]


uint64 : Fuzzer UInt64
uint64 =
    Fuzz.frequency
        [ ( 1, Fuzz.constant UInt64.zero )
        , ( 10, uint64NonZero )
        ]


uint64NonZero : Fuzzer UInt64
uint64NonZero =
    Fuzz.frequency
        [ -- firstly uniform by bitsize from 1 to 64
          ( 24, Fuzz.map (\a -> UInt64.fromInt24s 0 0 a) (uintUniformByBitsize 1 24) )
        , ( 24, Fuzz.map2 (\a b -> UInt64.fromInt24s 0 a b) (uintUniformByBitsize 1 24) (uintUniformByValue 24) )
        , ( 16
          , Fuzz.map3
                (\a b c -> UInt64.fromInt24s a b c)
                (uintUniformByBitsize 1 16)
                (uintUniformByValue 24)
                (uintUniformByValue 24)
          )

        -- then extra weight for smallest/largest bitsizes
        , ( 10, Fuzz.map (\a -> UInt64.fromInt24s 0 0 a) (uintUniformByBitsize 1 4) )
        , ( 10
          , Fuzz.map3
                (\a b c -> UInt64.fromInt24s a b c)
                (uintUniformByBitsize 13 16)
                (uintUniformByValue 24)
                (uintUniformByValue 24)
          )
        , ( 10, Fuzz.constant UInt64.maxValue )
        ]



-- FUZZERS - BASE


{-| Fuzzer of unsigned integers.

  - Uniform distribution by **bitsize**:
    Each bitsize from `minBits` to `maxBits` (both inclusive) has equal chance of being chosen.
      - Then, once bitsize has been chosen, each value within that bitsize has equal chance of being chosen.
  - Supports 0-31 bits
  - Undefined behavior if `minBits` > `maxBits`

-}
uintUniformByBitsize : Int -> Int -> Fuzzer Int
uintUniformByBitsize minBits maxBits =
    (Fuzz.oneOf << List.take (maxBits - minBits + 1) << List.drop minBits)
        [ Fuzz.constant 0x00
        , Fuzz.constant 0x01
        , Fuzz.intRange 0x02 0x03
        , Fuzz.intRange 0x04 0x07
        , Fuzz.intRange 0x08 0x0F
        , Fuzz.intRange 0x10 0x1F
        , Fuzz.intRange 0x20 0x3F
        , Fuzz.intRange 0x40 0x7F
        , Fuzz.intRange 0x80 0xFF
        , Fuzz.intRange 0x0100 0x01FF
        , Fuzz.intRange 0x0200 0x03FF
        , Fuzz.intRange 0x0400 0x07FF
        , Fuzz.intRange 0x0800 0x0FFF
        , Fuzz.intRange 0x1000 0x1FFF
        , Fuzz.intRange 0x2000 0x3FFF
        , Fuzz.intRange 0x4000 0x7FFF
        , Fuzz.intRange 0x8000 0xFFFF
        , Fuzz.intRange 0x00010000 0x0001FFFF
        , Fuzz.intRange 0x00020000 0x0003FFFF
        , Fuzz.intRange 0x00040000 0x0007FFFF
        , Fuzz.intRange 0x00080000 0x000FFFFF
        , Fuzz.intRange 0x00100000 0x001FFFFF
        , Fuzz.intRange 0x00200000 0x003FFFFF
        , Fuzz.intRange 0x00400000 0x007FFFFF
        , Fuzz.intRange 0x00800000 0x00FFFFFF
        , Fuzz.intRange 0x01000000 0x01FFFFFF
        , Fuzz.intRange 0x02000000 0x03FFFFFF
        , Fuzz.intRange 0x04000000 0x07FFFFFF
        , Fuzz.intRange 0x08000000 0x0FFFFFFF
        , Fuzz.intRange 0x10000000 0x1FFFFFFF
        , Fuzz.intRange 0x20000000 0x3FFFFFFF
        , Fuzz.intRange 0x40000000 0x7FFFFFFF
        ]


{-| Fuzzer of unsigned integers.

  - Uniform distribution by **value**:
    Each possible value from 0 so `2 ^ maxBits - 1` has equal chance of being chosen.
  - Supports 0-31 bits

-}
uintUniformByValue : Int -> Fuzzer Int
uintUniformByValue maxBits =
    Fuzz.intRange 0 (2 ^ maxBits - 1)



-- HELPERS - CHAR / STRING


{-| Convert `Int` to `Char` digit. Return `*` for values over 15.

Different implementation from UInt64.elm to compare against.

-}
riskyIntToCharDigit : Bool -> Int -> Char
riskyIntToCharDigit upper x =
    if x >= 0 && x <= 9 then
        Char.fromCode (x + Char.toCode '0')

    else if x >= 10 && x <= 15 then
        if upper then
            Char.fromCode (x - 10 + Char.toCode 'A')

        else
            Char.fromCode (x - 10 + Char.toCode 'a')

    else
        -- different character for invalid argument, compated to UInt64.elm
        '*'

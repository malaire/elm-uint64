module Tests exposing (all)

{-|


# One full-range-fuzz for every function

Every `UInt64` function, no matter how trivial, must have at least one full-range-fuzz test.
For each function one such test is marked with "-- FULL RANGE FUZZ: functionName".

If such a test is modified, it must be made sure that the function in question still has
one marked full-range-fuzz test, either the test in question, or some other test.

Constants are excluded as they aren't really functions and can't use fuzz input.


# Key functions

Following functions are considered key functions in regards to testing.
These are tested better so that other similar functions can then be tested against these key functions.

  - CONVERSION
      - fromBigEndianBytes
  - MATH
      - add

-}

import Bitwise
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Random
import SafeInt
import Shrink
import Test exposing (..)
import UInt64 exposing (UInt64)


all =
    describe "UInt64"
        [ -- CONSTANTS
          testConstants

        -- ARGUMENT HANDLING
        , test_limitFloat
        , test_limitLargeInt
        , test_limitSmallInt

        -- CONVERSION - INT
        , test_fromInt
        , test_toInt31
        , test_toInt53

        -- CONVERSION - FLOAT
        , test_floor
        , test_floor_toFloat
        , test_toFloat

        -- CONVERSION - PARTS
        , test_fromBigEndianBytes
        , test_fromDecimal12s
        , test_fromDecimal12s_toString
        , test_fromInt24s_fromBigEndianBytes
        , test_fromInt32s_fromBigEndianBytes
        , test_toBigEndianBytes_fromBigEndianBytes
        , test_toInt24s_fromInt24s
        , test_toInt32s_fromInt32s

        -- CONVERSION - STRING
        , test_fromString
        , test_toHexString
        , test_toHexString_fromBigEndianBytes

        -- MATH
        , test_add
        , test_add_mul
        , test_add_sub
        , test_decrement
        , test_decrement_add
        , test_increment
        , test_increment_add
        , test_mul
        , test_pow
        , test_sub
        , test_sub_add_complement
        , test_sub_mul

        -- DIVISION
        , test_div_mod
        , test_divMod
        , test_divMod_individual
        , test_divModFast
        , test_divModFast_individual
        , test_divModSlow
        , test_mod_individual

        -- BITWISE
        , test_and_or_xor_complement
        , test_getBit
        , test_rotateLeftBy_shiftLeftBy_shiftRightZfBy
        , test_rotateRightBy_shiftLeftBy_shiftRightZfBy
        , test_setBit
        , test_shiftLeftBy_mul
        , test_shiftRightZfBy_div

        -- COMPARISON
        , test_compare
        , test_isSafe

        -- INTERNALS
        , testInternals
        ]



-- TESTS - CONSTANTS


testConstants =
    describe "constants"
        [ test "maxFloat == 0xFFFFFFFFFFFFF800" <|
            \_ -> UInt64.maxFloat |> UInt64.toInt24s |> Expect.equal ( 0xFFFF, 0x00FFFFFF, 0x00FFF800 )
        , test "maxFloat == maxFloatAsFloat" <|
            \_ -> UInt64.maxFloat |> UInt64.toFloat |> Expect.equal UInt64.maxFloatAsFloat
        , test "maxSafe == 0x001FFFFFFFFFFFFF" <|
            \_ -> UInt64.maxSafe |> UInt64.toInt24s |> Expect.equal ( 0x1F, 0x00FFFFFF, 0x00FFFFFF )
        , test "maxSafe == maxSafeAsFloat" <|
            \_ -> UInt64.maxSafe |> UInt64.toFloat |> Expect.equal UInt64.maxSafeAsFloat
        , test "maxValue == 0xFFFFFFFFFFFFFFFF" <|
            \_ -> UInt64.maxValue |> UInt64.toInt24s |> Expect.equal ( 0xFFFF, 0x00FFFFFF, 0x00FFFFFF )
        , test "maxValue + one == minValue" <|
            \_ -> UInt64.add UInt64.maxValue UInt64.one |> Expect.equal UInt64.minValue
        , test "minValue == 0" <|
            \_ -> UInt64.minValue |> UInt64.toInt24s |> Expect.equal ( 0, 0, 0 )
        , test "minValue - one == maxValue" <|
            \_ -> UInt64.sub UInt64.minValue UInt64.one |> Expect.equal UInt64.maxValue
        , test "zero == 0" <|
            \_ -> UInt64.zero |> UInt64.toInt24s |> Expect.equal ( 0, 0, 0 )
        , test "one == 1" <|
            \_ -> UInt64.one |> UInt64.toInt24s |> Expect.equal ( 0, 0, 1 )
        , test "two == 2" <|
            \_ -> UInt64.two |> UInt64.toInt24s |> Expect.equal ( 0, 0, 2 )
        ]



-- TESTS - ARGUMENT HANDLING


test_limitFloat =
    describe "limitFloat"
        [ -- INDIVIDUAL
          test "value is rounded down" <|
            \_ -> UInt64.limitFloat 999 12.99 |> Expect.equal 12.0
        , test "max is rounded down" <|
            \_ -> UInt64.limitFloat 999.99 1000 |> Expect.equal 999.0
        , test "value is NaN" <|
            \_ -> UInt64.limitFloat 999 floatNaN |> Expect.equal 0.0
        , test "max is NaN" <|
            \_ -> UInt64.limitFloat floatNaN 999 |> Expect.equal 0.0
        , test "value < 0" <|
            \_ -> UInt64.limitFloat 999 -12.34 |> Expect.equal 0.0
        , test "max < 0, value > 0" <|
            \_ -> UInt64.limitFloat -12.34 12.34 |> Expect.equal 0.0
        , test "max < value < 0" <|
            \_ -> UInt64.limitFloat -12.34 -1.23 |> Expect.equal 0.0
        , test "value < max < 0" <|
            \_ -> UInt64.limitFloat -12.34 -123.45 |> Expect.equal 0.0
        , test "value > max > maxFloat" <|
            \_ -> UInt64.limitFloat 1.0e30 1.0e40 |> Expect.equal UInt64.maxFloatAsFloat
        , test "max > value > maxFloat" <|
            \_ -> UInt64.limitFloat 1.0e30 1.0e20 |> Expect.equal UInt64.maxFloatAsFloat

        -- FUZZ
        -- FULL RANGE FUZZ: limitFloat
        , fuzz2 (anyFloat UInt64.maxFloatAsFloat) (anyFloat UInt64.maxFloatAsFloat) "fuzz" <|
            \max value ->
                let
                    expectedUnfloored =
                        if Basics.isNaN max || Basics.isNaN value || max < 0 || value < 0 then
                            0.0

                        else if value > UInt64.maxFloatAsFloat || value > max then
                            if max > UInt64.maxFloatAsFloat then
                                UInt64.maxFloatAsFloat

                            else
                                max

                        else
                            value

                    expected =
                        if expectedUnfloored > minCertainInteger then
                            expectedUnfloored

                        else
                            Basics.toFloat <| Basics.floor expectedUnfloored
                in
                UInt64.limitFloat max value
                    |> Expect.equal expected
        ]


test_limitLargeInt =
    describe "limitLargeInt"
        [ -- INDIVIDUAL
          test "-1" <|
            \_ -> UInt64.limitLargeInt -1 |> Expect.equal 0
        , test "9007199254740991" <|
            \_ -> UInt64.limitLargeInt 9007199254740991 |> Expect.equal 9007199254740991
        , test "9007199254740992" <|
            \_ -> UInt64.limitLargeInt 9007199254740992 |> Expect.equal 9007199254740991

        -- FUZZ
        -- FULL RANGE FUZZ: limitLargeInt
        , fuzz largeInt "fuzz" <|
            -- this test doesn't make much sense, but can't think of anything useful here
            \value ->
                UInt64.limitLargeInt value
                    |> Expect.equal
                        (if value < 0 then
                            0

                         else if value > 9007199254740991 then
                            9007199254740991

                         else
                            value
                        )
        ]


test_limitSmallInt =
    describe "limitSmallInt"
        [ -- INDIVIDUAL
          test "-1, -1" <|
            \_ -> UInt64.limitSmallInt -1 -1 |> Expect.equal 0x7FFFFFFF
        , test "32, -1" <|
            \_ -> UInt64.limitSmallInt 32 -1 |> Expect.equal 0xFFFFFFFF

        -- FUZZ
        -- FULL RANGE FUZZ: limitSmallInt
        , fuzz2 smallInt smallInt "fuzz" <|
            \bitSize value ->
                let
                    limitWith64Math validBitSize_ value_ =
                        -- NOTE: "smallInt" can be outside safe range, so can't use `UInt64.fromInt` here
                        let
                            positiveValue_ =
                                if value_ < 0 then
                                    UInt64.floor (Basics.toFloat <| Basics.negate value_)
                                        |> UInt64.complement
                                        |> UInt64.increment

                                else
                                    UInt64.floor <| Basics.toFloat value_
                        in
                        UInt64.shiftLeftBy validBitSize_ UInt64.one
                            |> UInt64.decrement
                            |> UInt64.and positiveValue_
                            |> UInt64.toInt53

                    limitedBitSize =
                        case limitWith64Math 5 bitSize of
                            Just 0 ->
                                32

                            Just x ->
                                x

                            Nothing ->
                                -- INTERNAL ERROR
                                42
                in
                UInt64.limitSmallInt bitSize value
                    |> Just
                    |> Expect.equal (limitWith64Math limitedBitSize value)
        ]



-- TESTS - CONVERSION - INT


test_fromInt =
    describe "fromInt"
        [ -- INDIVIDUAL
          test "n < 0" <|
            \_ -> UInt64.fromInt -42 |> Expect.equal UInt64.zero
        , test "0 < n < 2^32" <|
            \_ -> UInt64.fromInt 111222333 |> UInt64.toString |> Expect.equal "111222333"
        , test "2^32 < n < maxSafe" <|
            \_ -> UInt64.fromInt 111222333444555 |> UInt64.toString |> Expect.equal "111222333444555"
        , test "maxSafe" <|
            \_ -> UInt64.fromInt 9007199254740991 |> Expect.equal UInt64.maxSafe
        , test "n > maxSafe" <|
            \_ -> UInt64.fromInt 9007199254740992 |> Expect.equal UInt64.maxSafe

        -- FUZZ
        -- FULL RANGE FUZZ: fromInt
        , fuzz largeInt "fromInt >> toString == String.fromInt" <|
            \x -> UInt64.fromInt x |> UInt64.toString |> Expect.equal (String.fromInt <| UInt64.limitLargeInt x)
        ]


test_toInt31 =
    describe "toInt31"
        [ -- INDIVIDUAL
          test "0x7FFFFFFF -> Just 0x7FFFFFFF" <|
            \_ -> UInt64.fromInt24s 0 0x7F 0x00FFFFFF |> UInt64.toInt31 |> Expect.equal (Just 0x7FFFFFFF)
        , test "0x80000000 -> Nothing" <|
            \_ -> UInt64.fromInt24s 0 0x80 0 |> UInt64.toInt31 |> Expect.equal Nothing

        -- FUZZ
        -- FULL RANGE FUZZ: toInt31
        , fuzz uint64 "compare to toFloat" <|
            \a ->
                let
                    asFloat =
                        UInt64.toFloat a

                    expected =
                        if asFloat > 0x7FFFFFFF then
                            Nothing

                        else
                            Just <| Basics.floor asFloat
                in
                UInt64.toInt31 a |> Expect.equal expected
        ]


test_toInt53 =
    describe "toInt53"
        [ -- INDIVIDUAL
          test "0x001FFFFFFFFFFFFF -> Just 0x001FFFFFFFFFFFFF" <|
            \_ -> UInt64.fromInt24s 0x1F 0x00FFFFFF 0x00FFFFFF |> UInt64.toInt53 |> Expect.equal (Just 0x001FFFFFFFFFFFFF)
        , test "0x0020000000000000 -> Nothing" <|
            \_ -> UInt64.fromInt24s 0x20 0 0 |> UInt64.toInt53 |> Expect.equal Nothing

        -- FUZZ
        -- FULL RANGE FUZZ: toInt53
        , fuzz uint64 "compare to toFloat" <|
            \a ->
                let
                    asFloat =
                        UInt64.toFloat a

                    expected =
                        if asFloat > UInt64.maxSafeAsFloat then
                            Nothing

                        else
                            Just <| Basics.floor asFloat
                in
                UInt64.toInt53 a |> Expect.equal expected
        ]



-- TESTS - CONVERSION - FLOAT


test_floor =
    describe "floor"
        [ test "NaN" <|
            \_ -> UInt64.floor floatNaN |> Expect.equal UInt64.zero
        , test "negative infinity" <|
            \_ -> UInt64.floor floatNegInf |> Expect.equal UInt64.zero
        , test "positive infinity" <|
            \_ -> UInt64.floor floatPosInf |> Expect.equal UInt64.maxValue
        , test "n < 0" <|
            \_ -> UInt64.floor -42 |> Expect.equal UInt64.zero
        , test "3.8" <|
            \_ -> UInt64.floor 3.8 |> UInt64.toString |> Expect.equal "3"
        , test "maxNonInteger" <|
            \_ -> UInt64.floor maxNonInteger |> UInt64.toString |> Expect.equal "4503599627370495"
        , test "maxFloatAsFloat" <|
            \_ -> UInt64.floor UInt64.maxFloatAsFloat |> UInt64.toString |> Expect.equal "18446744073709549568"
        , test "n > maxValue" <|
            \_ -> UInt64.floor 1.0e20 |> Expect.equal UInt64.maxValue
        ]


test_floor_toFloat =
    -- FULL RANGE FUZZ: floor
    fuzz (anyFloat UInt64.maxFloatAsFloat) "floor >> toFloat == id" <|
        \x ->
            UInt64.floor x
                |> UInt64.toFloat
                |> Expect.within (Expect.Absolute 0.0)
                    (if x >= 18446744073709552000.0 then
                        18446744073709552000.0

                     else
                        UInt64.limitFloat UInt64.maxFloatAsFloat x
                    )


test_toFloat =
    describe "toFloat"
        [ -- INDIVIDUAL
          test "maxValue" <|
            \_ ->
                UInt64.toFloat UInt64.maxValue
                    |> Expect.within (Expect.Absolute 0.0) 18446744073709552000.0

        -- FUZZ
        -- FULL RANGE FUZZ: toFloat
        , fuzz uint64 "compare to calculated result" <|
            \x ->
                -- simulate "round to nearest, ties to even" of IEEE 754 binary64
                -- NOTE: If this test fails, your CPU may be faulty.
                let
                    ( high, mid, low ) =
                        UInt64.toInt24s x

                    -- 0 to 11
                    scale =
                        if UInt64.isSafe x then
                            0

                        else
                            (Basics.floor <| Basics.logBase 2 <| Basics.toFloat high) - 4

                    highestDroppedBitMask =
                        Bitwise.shiftLeftBy (scale - 1) 1

                    lowestKeptBitMask =
                        Bitwise.shiftLeftBy scale 1

                    withDroppedBitsCleared =
                        UInt64.fromInt24s high mid (Bitwise.and (Bitwise.complement (lowestKeptBitMask - 1)) low)

                    roundByBit n =
                        if Bitwise.and (Bitwise.shiftLeftBy n 1) low == 0 then
                            -- round down
                            withDroppedBitsCleared

                        else
                            -- round up
                            withDroppedBitsCleared
                                |> UInt64.add (UInt64.fromInt lowestKeptBitMask)

                    rounded =
                        if scale == 0 then
                            x

                        else if
                            (Bitwise.and (highestDroppedBitMask - 1) low == 0)
                                && (Bitwise.and highestDroppedBitMask low /= 0)
                        then
                            -- is tie, round to even
                            roundByBit scale

                        else
                            -- not tie, round to nearest
                            roundByBit (scale - 1)

                    expected =
                        UInt64.shiftRightZfBy scale rounded
                            |> UInt64.toFloat
                            |> (*) (Basics.toFloat <| Bitwise.shiftLeftBy scale 1)

                    expectedFixed =
                        -- large values can overflow 64-bit integer, resulting in zero
                        if expected == 0.0 && scale /= 0 then
                            18446744073709552000.0

                        else
                            expected
                in
                UInt64.toFloat x |> Expect.within (Expect.Absolute 0.0) expectedFixed
        ]



-- TESTS - CONVERSION - PARTS


test_fromBigEndianBytes =
    describe "fromBigEndianBytes"
        [ -- INDIVIDUAL
          test "empty list" <|
            \_ -> UInt64.fromBigEndianBytes [] |> Expect.equal UInt64.zero

        -- FUZZ
        , fuzz (Fuzz.list smallInt) "prepending zero-bytes is no-op" <|
            \bytes ->
                UInt64.fromBigEndianBytes bytes
                    |> Expect.equal (UInt64.fromBigEndianBytes ([ 0, 0, 0, 0, 0, 0, 0, 0 ] ++ bytes))
        , fuzz (Fuzz.list smallInt) "`limitSmallInt 8` per byte is no-op" <|
            \bytes ->
                UInt64.fromBigEndianBytes bytes
                    |> Expect.equal (UInt64.fromBigEndianBytes (List.map (UInt64.limitSmallInt 8) bytes))
        , fuzz (Fuzz.list smallInt) "`takeLast 8` is no-op" <|
            let
                takeLast n list =
                    List.drop (List.length list - 8) list
            in
            \bytes ->
                UInt64.fromBigEndianBytes bytes
                    |> Expect.equal (UInt64.fromBigEndianBytes (takeLast 8 bytes))
        ]


test_fromDecimal12s =
    describe "fromDecimal12s"
        [ test "high is NaN" <|
            \_ -> UInt64.fromDecimal12s floatNaN 123 |> Expect.equal (UInt64.fromInt24s 0 0 123)
        , test "low is NaN" <|
            \_ -> UInt64.fromDecimal12s 123 floatNaN |> Expect.equal (UInt64.fromDecimal12s 123 0)
        , test "12.34 56.78" <|
            \_ -> UInt64.fromDecimal12s 12.34 56.78 |> UInt64.toString |> Expect.equal "12000000000056"
        , test "-1 -1" <|
            \_ -> UInt64.fromDecimal12s -1 -1 |> Expect.equal UInt64.zero
        , test "-1 100000000000" <|
            \_ -> UInt64.fromDecimal12s -1 100000000000 |> UInt64.toString |> Expect.equal "100000000000"
        , test "18446744 -1" <|
            \_ -> UInt64.fromDecimal12s 18446744 -1 |> UInt64.toString |> Expect.equal "18446744000000000000"
        , test "18446744 10000000000" <|
            \_ -> UInt64.fromDecimal12s 18446744 10000000000 |> UInt64.toString |> Expect.equal "18446744010000000000"
        , test "18446744 100000000000" <|
            \_ -> UInt64.fromDecimal12s 18446744 100000000000 |> Expect.equal UInt64.maxValue
        , test "18446745 -1" <|
            \_ -> UInt64.fromDecimal12s 18446745 -1 |> Expect.equal UInt64.maxValue
        ]


test_fromDecimal12s_toString =
    describe "fromDecimal12s & toString"
        [ -- FULL RANGE FUZZ: fromDecimal12s  ; augmented by "high is NaN" & "low is NaN" in test_fromDecimal12s
          -- FULL RANGE FUZZ: toString
          fuzz2 (anyFloat 18446745.0) (anyFloat 999999999999.0) "fromDecimal12s >> toString == generated value" <|
            \high low ->
                let
                    limitedHigh =
                        UInt64.limitFloat 18446745.0 high

                    limitedLow =
                        UInt64.limitFloat 999999999999.0 low

                    expected =
                        if limitedHigh == 0.0 then
                            String.fromInt <| Basics.floor limitedLow

                        else
                            (String.fromInt <| Basics.floor limitedHigh)
                                ++ (String.padLeft 12 '0' <| String.fromInt <| Basics.floor limitedLow)

                    expectedFixed =
                        if String.length expected == 20 && expected > "18446744073709551615" then
                            "18446744073709551615"

                        else
                            expected
                in
                UInt64.fromDecimal12s high low
                    |> UInt64.toString
                    |> Expect.equal expectedFixed
        ]


test_fromInt24s_fromBigEndianBytes =
    -- FULL RANGE FUZZ: fromInt24s
    fuzz3 smallInt smallInt smallInt "compare fromInt24s to fromBigEndianBytes" <|
        \high mid low ->
            let
                limitedHigh =
                    UInt64.limitSmallInt 16 high

                limitedMid =
                    UInt64.limitSmallInt 24 mid

                limitedLow =
                    UInt64.limitSmallInt 24 low

                bytes =
                    [ Bitwise.shiftRightZfBy 8 limitedHigh
                    , Bitwise.and 0xFF limitedHigh
                    , Bitwise.shiftRightZfBy 16 limitedMid
                    , Bitwise.and 0xFF <| Bitwise.shiftRightZfBy 8 limitedMid
                    , Bitwise.and 0xFF limitedMid
                    , Bitwise.shiftRightZfBy 16 limitedLow
                    , Bitwise.and 0xFF <| Bitwise.shiftRightZfBy 8 limitedLow
                    , Bitwise.and 0xFF limitedLow
                    ]
            in
            UInt64.fromInt24s high mid low
                |> Expect.equal (UInt64.fromBigEndianBytes bytes)


test_fromInt32s_fromBigEndianBytes =
    -- FULL RANGE FUZZ: fromInt32s
    fuzz2 smallInt smallInt "compare fromInt32s to fromBigEndianBytes" <|
        \high32 low32 ->
            let
                limitedHigh32 =
                    UInt64.limitSmallInt 32 high32

                limitedLow32 =
                    UInt64.limitSmallInt 32 low32

                bytes =
                    [ Bitwise.shiftRightZfBy 24 limitedHigh32
                    , Bitwise.and 0xFF <| Bitwise.shiftRightZfBy 16 limitedHigh32
                    , Bitwise.and 0xFF <| Bitwise.shiftRightZfBy 8 limitedHigh32
                    , Bitwise.and 0xFF limitedHigh32
                    , Bitwise.shiftRightZfBy 24 limitedLow32
                    , Bitwise.and 0xFF <| Bitwise.shiftRightZfBy 16 limitedLow32
                    , Bitwise.and 0xFF <| Bitwise.shiftRightZfBy 8 limitedLow32
                    , Bitwise.and 0xFF limitedLow32
                    ]
            in
            UInt64.fromInt32s high32 low32
                |> Expect.equal (UInt64.fromBigEndianBytes bytes)


test_toBigEndianBytes_fromBigEndianBytes =
    -- FULL RANGE FUZZ: toBigEndianBytes
    fuzz uint64 "toBigEndianBytes >> ... >> fromBigEndianBytes == id" <|
        \a ->
            UInt64.toBigEndianBytes a
                |> (\bytes ->
                        if List.all (\byte -> byte >= 0 && byte <= 255) bytes then
                            Ok <| UInt64.fromBigEndianBytes bytes

                        else
                            Err "Invalid output"
                   )
                |> Expect.equal (Ok a)


test_toInt24s_fromInt24s =
    -- FULL RANGE FUZZ: toInt24s
    fuzz uint64 "toInt24s >> ... >> fromInt24s == id" <|
        \a ->
            UInt64.toInt24s a
                |> (\( high, mid, low ) ->
                        if
                            (high == Bitwise.and 0xFFFF high)
                                && (mid == Bitwise.and 0x00FFFFFF mid)
                                && (low == Bitwise.and 0x00FFFFFF low)
                        then
                            Ok <| UInt64.fromInt24s high mid low

                        else
                            Err "Invalid output"
                   )
                |> Expect.equal (Ok a)


test_toInt32s_fromInt32s =
    -- FULL RANGE FUZZ: toInt32s
    fuzz uint64 "toInt32s >> ... >> fromInt32s == id" <|
        \a ->
            UInt64.toInt32s a
                |> (\( high32, low32 ) ->
                        if
                            (high32 == Bitwise.shiftRightZfBy 0 high32)
                                && (low32 == Bitwise.shiftRightZfBy 0 low32)
                        then
                            Ok <| UInt64.fromInt32s high32 low32

                        else
                            Err "Invalid output"
                   )
                |> Expect.equal (Ok a)



-- TESTS - CONVERSION - STRING


test_fromString =
    let
        anyCharFuzzer : Fuzzer Char
        anyCharFuzzer =
            Fuzz.frequency
                [ -- NOTE: Don't generate lone high/low surrogates as this seems to cause problems with `elm-test`.
                  --       Such invalid String:s are possible in Elm, and so I'd like to test them,
                  --       but that doesn't seem possible for now.
                  ( 3, Fuzz.map Char.fromCode <| Fuzz.intRange 0 0xD7FF )
                , ( 1, Fuzz.map Char.fromCode <| Fuzz.intRange 0xE000 0xFFFF )
                , ( 3, Fuzz.map Char.fromCode <| Fuzz.intRange 0x00100000 0x0010FFFF )
                ]

        withoutPrefixFuzzer : Dict Char a -> Fuzzer String
        withoutPrefixFuzzer chars =
            -- For String of 10 characters, this gives about 50/50 chance of it containing at least one incorrect char.
            Fuzz.frequency
                [ ( 93, Fuzz.oneOf <| List.map Fuzz.constant <| Dict.keys chars )
                , ( 7, anyCharFuzzer )
                ]
                |> Fuzz.list
                |> Fuzz.map String.fromList

        withPrefixFuzzer : Fuzzer String
        withPrefixFuzzer =
            -- This seems to occasionally generate valid strings.
            let
                prefixChar =
                    Fuzz.frequency
                        [ ( 2, Fuzz.constant '0' )
                        , ( 2, Fuzz.constant '1' )
                        , ( 31, Fuzz.constant 'x' )
                        , ( 31, Fuzz.constant 'o' )
                        , ( 31, Fuzz.constant 'b' )
                        , ( 3, anyCharFuzzer )
                        ]

                list =
                    Fuzz.frequency
                        [ ( 62, Fuzz.constant '0' )
                        , ( 31, Fuzz.constant '1' )
                        , ( 1, Fuzz.constant 'x' )
                        , ( 1, Fuzz.constant 'o' )
                        , ( 1, Fuzz.constant 'b' )
                        , ( 4, anyCharFuzzer )
                        ]
                        |> Fuzz.list
            in
            Fuzz.map3 (\a b c -> String.fromList <| a ++ [ b ] ++ c) list prefixChar list
    in
    describe "fromString"
        [ -- INDIVIDUAL
          test "empty string" <|
            \_ -> UInt64.fromString "" |> Expect.equal Nothing

        -- INDIVIDUAL - DECIMAL
        , test "0" <|
            \_ -> UInt64.fromString "0" |> Expect.equal (Just UInt64.zero)
        , test "a lot of zeroes" <|
            \_ -> UInt64.fromString "0000000000000000000000000000000000000000" |> Expect.equal (Just UInt64.zero)
        , test "1 + a lot of leading zeroes" <|
            \_ -> UInt64.fromString "00000000000000000000000000000000000000001" |> Expect.equal (Just UInt64.one)
        , test "1 + a lot of trailing zeroes" <|
            \_ -> UInt64.fromString "10000000000000000000000000000000000000000" |> Expect.equal Nothing
        , test "maxValue" <|
            \_ -> UInt64.fromString "18446744073709551615" |> Expect.equal (Just UInt64.maxValue)
        , test "maxValue with a lot of leading zeroes" <|
            \_ -> UInt64.fromString "0000000000000000000018446744073709551615" |> Expect.equal (Just UInt64.maxValue)
        , test "maxValue + 1" <|
            \_ -> UInt64.fromString "18446744073709551616" |> Expect.equal Nothing
        , test "highDecimal == 1844674408" <|
            \_ -> UInt64.fromString "18446744080000000000" |> Expect.equal Nothing

        -- INDIVIDUAL - HEX
        , test "x" <|
            \_ -> UInt64.fromString "x" |> Expect.equal Nothing
        , test "x0" <|
            \_ -> UInt64.fromString "x0" |> Expect.equal Nothing
        , test "0x" <|
            \_ -> UInt64.fromString "0x" |> Expect.equal Nothing
        , test "0x0" <|
            \_ -> UInt64.fromString "0x0" |> Expect.equal (Just UInt64.zero)
        , test "00x0" <|
            \_ -> UInt64.fromString "00x0" |> Expect.equal Nothing
        , test "hex, a lot of zeroes" <|
            \_ -> UInt64.fromString "0x0000000000000000000000000000000000000000" |> Expect.equal (Just UInt64.zero)
        , test "hex, mixed case" <|
            \_ -> UInt64.fromString "0xaAbBcCdDeEfF" |> Expect.equal (Just <| UInt64.fromInt24s 0 0x00AABBCC 0x00DDEEFF)
        , test "hex, maxValue" <|
            \_ -> UInt64.fromString "0xFFFFFFFFFFFFFFFF" |> Expect.equal (Just UInt64.maxValue)
        , test "hex, maxValue with a lot of leading zeroes" <|
            \_ -> UInt64.fromString "0x00000000000000000000FFFFFFFFFFFFFFFF" |> Expect.equal (Just UInt64.maxValue)
        , test "hex, maxValue + 1" <|
            \_ -> UInt64.fromString "0x10000000000000000" |> Expect.equal Nothing

        -- INDIVIDUAL - OCTAL
        , test "0o" <|
            \_ -> UInt64.fromString "0o" |> Expect.equal Nothing
        , test "octal, with digit 8" <|
            \_ -> UInt64.fromString "0o787" |> Expect.equal Nothing
        , test "octal, maxValue" <|
            \_ -> UInt64.fromString "0o1777777777777777777777" |> Expect.equal (Just UInt64.maxValue)
        , test "octal, maxValue + 1" <|
            \_ -> UInt64.fromString "0o2000000000000000000000" |> Expect.equal Nothing

        -- INDIVIDUAL - BINARY
        , test "0b" <|
            \_ -> UInt64.fromString "0b" |> Expect.equal Nothing
        , test "binary, with digit 2" <|
            \_ -> UInt64.fromString "0b121" |> Expect.equal Nothing
        , test "binary, maxValue" <|
            \_ ->
                UInt64.fromString "0b1111111111111111111111111111111111111111111111111111111111111111"
                    |> Expect.equal (Just UInt64.maxValue)
        , test "binary, maxValue + 1" <|
            \_ ->
                UInt64.fromString "0b10000000000000000000000000000000000000000000000000000000000000000"
                    |> Expect.equal Nothing

        -- FUZZ
        -- FULL RANGE FUZZ: fromString  ; these all fuzz together
        , fuzz2 uint64 (Fuzz.list <| Fuzz.constant '0') "fuzz - valid decimal" <|
            \a zeroes ->
                (String.fromList zeroes ++ UInt64.toString a)
                    |> UInt64.fromString
                    |> Expect.equal (Just a)
        , fuzz2 uint64 (Fuzz.list <| Fuzz.constant '0') "fuzz - valid hex" <|
            \a zeroes ->
                ("0x" ++ String.fromList zeroes ++ UInt64.toHexString a)
                    |> UInt64.fromString
                    |> Expect.equal (Just a)
        , fuzz2 uint64 (Fuzz.list <| Fuzz.constant '0') "fuzz - valid octal" <|
            \a zeroes ->
                ("0o" ++ String.fromList zeroes ++ toBinaryOrOctalString (UInt64.fromInt 8) "" a)
                    |> UInt64.fromString
                    |> Expect.equal (Just a)
        , fuzz2 uint64 (Fuzz.list <| Fuzz.constant '0') "fuzz - valid binary" <|
            \a zeroes ->
                ("0b" ++ String.fromList zeroes ++ toBinaryOrOctalString UInt64.two "" a)
                    |> UInt64.fromString
                    |> Expect.equal (Just a)
        , fuzz (withoutPrefixFuzzer charToDecimalDigit) "fuzz - valid/invalid decimal" <|
            \str -> UInt64.fromString str |> Expect.equal (alternativeFromString str)
        , fuzz (withoutPrefixFuzzer charToHexDigit) "fuzz - valid/invalid hex with valid prefix" <|
            \str -> UInt64.fromString ("0x" ++ str) |> Expect.equal (alternativeFromString ("0x" ++ str))
        , fuzz (withoutPrefixFuzzer charToOctalDigit) "fuzz - valid/invalid octal with valid prefix" <|
            \str -> UInt64.fromString ("0o" ++ str) |> Expect.equal (alternativeFromString ("0o" ++ str))
        , fuzz (withoutPrefixFuzzer charToBinaryDigit) "fuzz - valid/invalid binary with valid prefix" <|
            \str -> UInt64.fromString ("0b" ++ str) |> Expect.equal (alternativeFromString ("0b" ++ str))
        , fuzz withPrefixFuzzer "fuzz - mostly invalid anything" <|
            \str -> UInt64.fromString str |> Expect.equal (alternativeFromString str)
        ]


test_toHexString =
    describe "toHexString"
        [ test "0" <|
            \_ -> UInt64.zero |> UInt64.toHexString |> Expect.equal "0000000000000000"
        , test "1" <|
            \_ -> UInt64.one |> UInt64.toHexString |> Expect.equal "0000000000000001"
        ]


test_toHexString_fromBigEndianBytes =
    -- FULL RANGE FUZZ: fromBigEndianBytes
    -- FULL RANGE FUZZ: toHexString
    fuzz (Fuzz.list smallInt) "fromBigEndianBytes >> toHexString == generated hex" <|
        \bytes ->
            UInt64.fromBigEndianBytes bytes
                |> UInt64.toHexString
                |> Expect.equal
                    (let
                        byteCount =
                            List.length bytes
                     in
                     (List.repeat (8 - byteCount) 0 ++ bytes)
                        |> List.drop (byteCount - 8)
                        |> List.concatMap
                            (\byte ->
                                let
                                    limitedByte =
                                        UInt64.limitSmallInt 8 byte
                                in
                                [ nibbleToHex <| Bitwise.shiftRightZfBy 4 limitedByte
                                , nibbleToHex <| Bitwise.and 0x0F limitedByte
                                ]
                            )
                        |> String.fromList
                    )



-- TESTS - MATH


test_add =
    describe "add"
        [ fuzz2 uint52 uint52 "add vs SafeInt.add ; 52 bits" <|
            \a b ->
                UInt64.add (UInt64.fromInt a) (UInt64.fromInt b)
                    |> UInt64.toFloat
                    |> Just
                    |> Expect.equal
                        (SafeInt.add (SafeInt.new a) (SafeInt.new b)
                            |> SafeInt.toFloat
                        )
        , fuzz2 uint64 uint64 "a + b == b + a" <|
            \a b -> UInt64.add a b |> Expect.equal (UInt64.add b a)
        , fuzz3 uint64 uint64 uint64 "a + (b + c) == (a + b) + c" <|
            \a b c -> UInt64.add a (UInt64.add b c) |> Expect.equal (UInt64.add (UInt64.add a b) c)
        ]


test_add_mul =
    -- FULL RANGE FUZZ: add
    -- FULL RANGE FUZZ: mul
    describe "add & mul"
        [ fuzz3 uint64 uint64 uint64 "a * (b + c) == a * b + a * c" <|
            \a b c -> UInt64.mul a (UInt64.add b c) |> Expect.equal (UInt64.add (UInt64.mul a b) (UInt64.mul a c))
        ]


test_add_sub =
    describe "add & sub"
        [ fuzz2 uint64 uint64 "a + b - a == b" <|
            \a b -> UInt64.sub (UInt64.add a b) a |> Expect.equal b
        ]


test_decrement =
    describe "decrement"
        [ test "low overflow" <|
            \_ -> UInt64.decrement (UInt64.fromInt24s 0 0 0) |> Expect.equal UInt64.maxValue
        , test "mid overflow" <|
            \_ -> UInt64.decrement (UInt64.fromInt24s 0 1 0) |> Expect.equal (UInt64.fromInt24s 0 0 0x00FFFFFF)
        , test "high overflow" <|
            \_ -> UInt64.decrement (UInt64.fromInt24s 1 0 0) |> Expect.equal (UInt64.fromInt24s 0 0x00FFFFFF 0x00FFFFFF)
        ]


test_decrement_add =
    describe "decrement & add"
        [ -- FULL RANGE FUZZ: decrement
          fuzz uint64 "decrement a == add a maxValue" <|
            \a -> UInt64.decrement a |> Expect.equal (UInt64.add a UInt64.maxValue)
        ]


test_increment =
    describe "increment"
        [ test "low overflow" <|
            \_ -> UInt64.increment (UInt64.fromInt24s 0 0 0x00FFFFFF) |> Expect.equal (UInt64.fromInt24s 0 1 0)
        , test "mid overflow" <|
            \_ -> UInt64.increment (UInt64.fromInt24s 0 0x00FFFFFF 0x00FFFFFF) |> Expect.equal (UInt64.fromInt24s 1 0 0)
        , test "high overflow" <|
            \_ -> UInt64.increment UInt64.maxValue |> Expect.equal (UInt64.fromInt24s 0 0 0)
        ]


test_increment_add =
    describe "increment & add"
        [ -- FULL RANGE FUZZ: increment
          fuzz uint64 "increment a == add a 1" <|
            \a -> UInt64.increment a |> Expect.equal (UInt64.add a UInt64.one)
        ]


test_mul =
    describe "mul"
        [ fuzz2 uint64 uint64 "a * b == b * a" <|
            \a b -> UInt64.mul a b |> Expect.equal (UInt64.mul b a)
        , fuzz3 uint64 uint64 uint64 "a * (b * c) == (a * b) * c" <|
            \a b c -> UInt64.mul a (UInt64.mul b c) |> Expect.equal (UInt64.mul (UInt64.mul a b) c)
        ]


test_pow =
    let
        powFuzzTest baseFuzzer exponentFuzzer description =
            fuzz2 baseFuzzer exponentFuzzer description <|
                \base exponent ->
                    UInt64.pow base exponent
                        |> Expect.equal
                            (if exponent == UInt64.zero then
                                -- proof by induction - base case: exponent == 0
                                UInt64.one

                             else
                                -- proof by induction - induction step: if OK for (exponent - 1), then OK for exponent
                                UInt64.mul base <| UInt64.pow base <| UInt64.decrement exponent
                            )
    in
    describe "pow"
        [ -- INDIVIDUAL
          test "0^0 = 1" <|
            \_ -> UInt64.pow UInt64.zero UInt64.zero |> Expect.equal UInt64.one
        , test "0^1 = 0" <|
            \_ -> UInt64.pow UInt64.zero UInt64.one |> Expect.equal UInt64.zero
        , test "0^99 = 0" <|
            \_ -> UInt64.pow UInt64.zero (UInt64.fromInt 99) |> Expect.equal UInt64.zero
        , test "1^0 = 1" <|
            \_ -> UInt64.pow UInt64.one UInt64.zero |> Expect.equal UInt64.one
        , test "1^1 = 1" <|
            \_ -> UInt64.pow UInt64.one UInt64.one |> Expect.equal UInt64.one
        , test "1^99 = 1" <|
            \_ -> UInt64.pow UInt64.one (UInt64.fromInt 99) |> Expect.equal UInt64.one
        , test "2^63 = 0x8000000000000000" <|
            \_ -> UInt64.pow UInt64.two (UInt64.fromInt 63) |> Expect.equal (UInt64.fromInt24s 0x8000 0 0)
        , test "2^64 = 0 (overflow)" <|
            \_ -> UInt64.pow UInt64.two (UInt64.fromInt 64) |> Expect.equal UInt64.zero
        , test "3^40 = 0xA8B8B452291FE821" <|
            \_ -> UInt64.pow (UInt64.fromInt 3) (UInt64.fromInt 40) |> Expect.equal (UInt64.fromInt24s 0xA8B8 0x00B45229 0x001FE821)
        , test "3^100 = 0xD6947D55CF3813D1 (overflow)" <|
            \_ -> UInt64.pow (UInt64.fromInt 3) (UInt64.fromInt 100) |> Expect.equal (UInt64.fromInt24s 0xD694 0x007D55CF 0x003813D1)
        , test "3^10000000 = 0x559F5FCCA357201 (overflow)" <|
            \_ ->
                UInt64.pow (UInt64.fromInt 3) (UInt64.fromInt 10000000)
                    |> Expect.equal (UInt64.fromInt24s 0x0559 0x00F5FCCA 0x00357201)
        , test "3 ^ 10 ^ 19 = 12038004833498693633 (overflow)" <|
            \_ ->
                UInt64.pow (UInt64.fromInt 10) (UInt64.fromInt 19)
                    |> UInt64.pow (UInt64.fromInt 3)
                    |> UInt64.toString
                    |> Expect.equal "12038004833498693633"
        , describe "3^x ; 0 <= x <= 16" <|
            List.indexedMap
                (\n expected ->
                    test (String.fromInt n) <|
                        \_ -> UInt64.pow (UInt64.fromInt 3) (UInt64.fromInt n) |> UInt64.toInt53 |> Expect.equal (Just expected)
                )
                [ 1, 3, 9, 27, 81, 243, 729, 2187, 6561, 19683, 59049, 177147, 531441, 1594323, 4782969, 14348907, 43046721 ]

        -- FUZZ
        -- FULL RANGE FUZZ: pow  ; these three fuzz together
        , powFuzzTest uint64 uint64 "x^n == x * x^(n-1)"
        , powFuzzTest uint64 (Fuzz.map (UInt64.fromInt24s 0 0) <| Fuzz.intRange 0 32) "x^n == x * x^(n-1) ; small exponent"
        , powFuzzTest (Fuzz.map (UInt64.fromInt24s 0 0) <| Fuzz.intRange 0 5) uint64 "x^n == x * x^(n-1) ; small base"
        ]


test_sub =
    describe "sub"
        [ fuzz2 uint53 uint53 "sub vs SafeInt.sub ; 53 bits" <|
            \a b ->
                let
                    ( larger, smaller ) =
                        if a > b then
                            ( a, b )

                        else
                            ( b, a )
                in
                UInt64.sub (UInt64.fromInt larger) (UInt64.fromInt smaller)
                    |> UInt64.toFloat
                    |> Just
                    |> Expect.equal
                        (SafeInt.sub (SafeInt.new larger) (SafeInt.new smaller)
                            |> SafeInt.toFloat
                        )
        ]


test_sub_add_complement =
    -- FULL RANGE FUZZ: sub
    describe "sub & add & complement"
        [ fuzz2 uint64 uint64 "sub a b == add a (add (complement b) 1)" <|
            \a b -> UInt64.sub a b |> Expect.equal (UInt64.add a (UInt64.add (UInt64.complement b) UInt64.one))
        ]


test_sub_mul =
    describe "sub & mul"
        [ fuzz3 uint64 uint64 uint64 "a * (b - c) == a * b - a * c" <|
            \a b c -> UInt64.mul a (UInt64.sub b c) |> Expect.equal (UInt64.sub (UInt64.mul a b) (UInt64.mul a c))
        ]



-- TESTS - DIVISION - SHARED


testsForDivMod :
    (UInt64 -> UInt64 -> Result String ( UInt64, UInt64 ))
    -> List Test
testsForDivMod divModFn =
    let
        testDivModFn dividend divisor =
            divModFn dividend divisor
                |> (\result ->
                        case result of
                            Err error ->
                                Err error

                            Ok ( div_, mod_ ) ->
                                if div_ == UInt64.zero && mod_ == UInt64.zero then
                                    Ok UInt64.zero

                                else if UInt64.compare mod_ divisor == LT then
                                    Ok <| UInt64.add (UInt64.mul div_ divisor) mod_

                                else
                                    Err "mod_ >= divisor @ Tests.testsForDivMod"
                   )
                |> Expect.equal
                    (if divisor == UInt64.zero then
                        Ok UInt64.zero

                     else
                        Ok dividend
                    )
    in
    [ -- FULL RANGE FUZZ: <divModFn>
      fuzz2 uint64 uint64 "mod_ < divisor && div_ * divisor + mod_ == dividend" <|
        \dividend divisor -> testDivModFn dividend divisor
    , fuzz uint64 "mod_ < divisor && div_ * divisor + mod_ == dividend ; dividend = maxValue" <|
        \divisor -> testDivModFn UInt64.maxValue divisor
    ]



-- TESTS - DIVISION


test_div_mod =
    -- FULL RANGE FUZZ: div
    -- FULL RANGE FUZZ: mod
    describe "div & mod" <| testsForDivMod (\a b -> Ok ( UInt64.div a b, UInt64.mod a b ))


test_divMod =
    -- FULL RANGE FUZZ: divMod
    describe "divMod" <| testsForDivMod (\a b -> UInt64.divMod a b |> Ok)


test_divMod_individual =
    describe "divMod - individual"
        [ test "divMod maxValue 30-bit ; fails with `divisor < 2^29` algorithm" <|
            \_ ->
                UInt64.divMod UInt64.maxValue (UInt64.fromInt24s 0 0x20 0x004CB631)
                    |> Expect.equal ( UInt64.fromInt24s 0 2028 16777197, UInt64.fromInt24s 0 8 11634082 )
        , test "divMod 54-bit 49-bit ; fails with `dividend < 2^53 && divisor >= 2^29` algorithm" <|
            \_ ->
                UInt64.divMod (UInt64.fromInt24s 0x20 0 1) (UInt64.fromInt24s 1 0 0)
                    |> Expect.equal ( UInt64.fromInt24s 0 0 0x20, UInt64.one )
        ]


test_divModFast =
    -- FULL RANGE FUZZ: divModFast
    describe "divModFast" <| testsForDivMod UInt64.divModFast


test_divModFast_individual =
    describe "divModFast - cases related to approxDivMultiplied72"
        [ test "approxDivMultiplied72 == 2 ^ 64 ; 2020-06-10" <|
            \_ ->
                UInt64.divModFast UInt64.maxValue (UInt64.fromInt24s 0 2097152 0)
                    |> Expect.equal
                        (Ok <| ( UInt64.fromInt 524287, UInt64.fromInt24s 0 2097151 16777215 ))
        , test "approxDivMultiplied72 > 2 ^ 64 ; 2020-06-10" <|
            \_ ->
                UInt64.divModFast UInt64.maxValue (UInt64.fromInt24s 0 262143 14680065)
                    |> Expect.equal
                        (Ok <| ( UInt64.fromInt24s 0 0 4194305, UInt64.fromInt24s 0 262143 14680062 ))
        , test "approxDivMultiplied72 == dividend" <|
            \_ ->
                UInt64.divModFast (UInt64.fromInt 4) (UInt64.fromInt 1)
                    |> Expect.equal
                        (Ok <| ( UInt64.fromInt 4, UInt64.zero ))
        , describe "approxDivMultiplied72 < dividend"
            [ test "delta == divisor" <|
                \_ ->
                    UInt64.divModFast (UInt64.fromInt24s 256 0 2930386) (UInt64.fromInt 2)
                        |> Expect.equal
                            (Ok <| ( UInt64.fromInt24s 128 0 1465193, UInt64.zero ))
            , test "delta < divisor" <|
                \_ ->
                    UInt64.divModFast (UInt64.fromInt 1) (UInt64.fromInt 2)
                        |> Expect.equal
                            (Ok <| ( UInt64.zero, UInt64.fromInt 1 ))
            , test "delta > divisor && secondApproxDivMultiplied == dividend" <|
                \_ ->
                    UInt64.divModFast UInt64.maxValue (UInt64.fromInt 15)
                        |> Expect.equal
                            (Ok <| ( UInt64.fromInt24s 4369 1118481 1118481, UInt64.zero ))
            , test "delta > divisor && secondApproxDivMultiplied < dividend && secondDelta < divisor" <|
                \_ ->
                    UInt64.divModFast UInt64.maxValue (UInt64.fromInt 9)
                        |> Expect.equal
                            (Ok <| ( UInt64.fromInt24s 7281 13048945 13048945, UInt64.fromInt 6 ))
            ]
        , describe "approxDivMultiplied72 > dividend"
            [ test "delta72 < divisor; 2020-06-10" <|
                \_ ->
                    UInt64.divModFast (UInt64.fromInt24s 16384 262143 16776704) (UInt64.fromInt24s 0 262144 0)
                        |> Expect.equal
                            (Ok <| ( UInt64.fromInt 1048576, UInt64.fromInt24s 0 262143 16776704 ))
            , test "delta72 == divisor ; 2020-06-10" <|
                \_ ->
                    UInt64.divModFast UInt64.maxValue (UInt64.fromInt 1923)
                        |> Expect.equal
                            (Ok <| ( UInt64.fromInt24s 34 1343573 3358933, UInt64.zero ))
            , test "delta72 > divisor && deltaMod == 0 ; 2020-06-10" <|
                \_ ->
                    UInt64.divModFast UInt64.maxValue (UInt64.fromInt 5)
                        |> Expect.equal
                            (Ok <| ( UInt64.fromInt24s 13107 3355443 3355443, UInt64.zero ))
            , test "delta72 > divisor && deltaMod /= 0 ; 2020-06-10" <|
                \_ ->
                    UInt64.divModFast UInt64.maxValue (UInt64.fromInt 972)
                        |> Expect.equal
                            (Ok <| ( UInt64.fromInt24s 67 7111330 4004438, UInt64.fromInt 375 ))
            ]
        ]


test_divModSlow =
    -- FULL RANGE FUZZ: divModSlow
    describe "divModSlow" <| testsForDivMod (\a b -> UInt64.divModSlow a b |> Ok)


test_mod_individual =
    describe "mod"
        [ test "mod maxValue 30-bit ; fails with `divisor < 2^29` algorithm" <|
            \_ ->
                UInt64.mod UInt64.maxValue (UInt64.fromInt24s 0 0x20 0x004CB631)
                    |> Expect.equal (UInt64.fromInt24s 0 8 11634082)
        , test "mod 54-bit 49-bit ; fails with `dividend < 2^53 && divisor >= 2^29` algorithm" <|
            \_ ->
                UInt64.mod (UInt64.fromInt24s 0x20 0 1) (UInt64.fromInt24s 1 0 0)
                    |> Expect.equal UInt64.one
        ]



-- TESTS - BITWISE


test_and_or_xor_complement =
    let
        -- 0x55 = 0b01010101
        inputA =
            UInt64.fromInt24s 0x55 0x5500 0x00550000

        -- 0x33 = 0b00110011
        inputB =
            UInt64.fromInt24s 0x33 0x3300 0x00330000
    in
    describe "and, or, xor, complement"
        [ -- INDIVIDUAL
          test "and 0x55 0x33 -> 0x11" <|
            \_ -> UInt64.and inputA inputB |> Expect.equal (UInt64.fromInt24s 0x11 0x1100 0x00110000)
        , test "or 0x55 0x33 -> 0x77" <|
            \_ -> UInt64.or inputA inputB |> Expect.equal (UInt64.fromInt24s 0x77 0x7700 0x00770000)
        , test "xor 0x55 0x33 -> 0x66" <|
            \_ -> UInt64.xor inputA inputB |> Expect.equal (UInt64.fromInt24s 0x66 0x6600 0x00660000)
        , test "complement 0x55 -> 0xAA" <|
            \_ -> UInt64.complement inputA |> Expect.equal (UInt64.fromInt24s 0xFFAA 0x00FFAAFF 0x00AAFFFF)

        -- FUZZ
        -- https://en.wikipedia.org/wiki/Boolean_algebra#Monotone_laws
        -- FULL RANGE FUZZ: and  ; these two fuzz together
        , fuzz uint64 "a and 1 == a" <|
            \a -> UInt64.and a UInt64.maxValue |> Expect.equal a
        , fuzz uint64 "a and 0 == 0" <|
            \a -> UInt64.and a UInt64.zero |> Expect.equal UInt64.zero

        -- FULL RANGE FUZZ: or  ; these two fuzz together
        , fuzz uint64 "a or 0 == a" <|
            \a -> UInt64.or a UInt64.zero |> Expect.equal a
        , fuzz uint64 "a or 1 == 1" <|
            \a -> UInt64.or a UInt64.maxValue |> Expect.equal UInt64.maxValue

        -- https://en.wikipedia.org/wiki/Boolean_algebra#Nonmonotone_laws
        -- > "All properties of negation ... follow from the ... two laws alone."
        -- FULL RANGE FUZZ: complement  ; these two fuzz together
        , fuzz uint64 "a and (complement a) == 0" <|
            \a -> UInt64.and a (UInt64.complement a) |> Expect.equal UInt64.zero
        , fuzz uint64 "a or (complement a) == 1" <|
            \a -> UInt64.or a (UInt64.complement a) |> Expect.equal UInt64.maxValue

        -- https://en.wikipedia.org/wiki/Boolean_algebra#Secondary_operations
        -- FULL RANGE FUZZ: xor
        , fuzz2 uint64 uint64 "a xor b == (a or b) and (complement (a and b))" <|
            \a b ->
                UInt64.xor a b
                    |> Expect.equal
                        (UInt64.and
                            (UInt64.or a b)
                            (UInt64.complement <| UInt64.and a b)
                        )
        , fuzz2 uint64 uint64 "a xor b == (a and (complement b)) or ((complement a) and b)" <|
            \a b ->
                UInt64.xor a b
                    |> Expect.equal
                        (UInt64.or
                            (UInt64.and a (UInt64.complement b))
                            (UInt64.and (UInt64.complement a) b)
                        )
        ]


test_getBit =
    -- FULL RANGE FUZZ: getBit
    fuzz2 smallInt uint64 "getBit" <|
        \n a ->
            UInt64.getBit n a
                |> Expect.equal
                    (UInt64.shiftRightZfBy n a
                        |> UInt64.and UInt64.one
                        |> UInt64.toInt31
                        |> Maybe.withDefault 42
                    )


test_rotateLeftBy_shiftLeftBy_shiftRightZfBy =
    -- FULL RANGE FUZZ: rotateLeftBy
    fuzz2 smallInt uint64 "rotateLeftBy vs. implementation with shiftLeftBy & shiftRightZfBy" <|
        \shift a ->
            UInt64.rotateLeftBy shift a
                |> Expect.equal
                    (let
                        limitedShift =
                            UInt64.limitSmallInt 6 shift
                     in
                     UInt64.or
                        (UInt64.shiftLeftBy limitedShift a)
                        (UInt64.shiftRightZfBy (64 - limitedShift) a)
                    )


test_rotateRightBy_shiftLeftBy_shiftRightZfBy =
    -- FULL RANGE FUZZ: rotateRightBy
    fuzz2 smallInt uint64 "rotateRightBy vs. implementation with shiftLeftBy & shiftRightZfBy" <|
        \shift a ->
            UInt64.rotateRightBy shift a
                |> Expect.equal
                    (let
                        limitedShift =
                            UInt64.limitSmallInt 6 shift
                     in
                     UInt64.or
                        (UInt64.shiftRightZfBy limitedShift a)
                        (UInt64.shiftLeftBy (64 - limitedShift) a)
                    )


test_setBit =
    -- FULL RANGE FUZZ: setBit
    fuzz3 smallInt smallInt uint64 "setBit" <|
        \bitNumber bitValue a ->
            UInt64.setBit bitNumber bitValue a
                |> Expect.equal
                    (let
                        limitedBitValue =
                            UInt64.limitSmallInt 1 bitValue
                     in
                     if limitedBitValue == 0 then
                        UInt64.shiftLeftBy bitNumber UInt64.one
                            |> UInt64.complement
                            |> UInt64.and a

                     else
                        UInt64.shiftLeftBy bitNumber UInt64.one
                            |> UInt64.or a
                    )


test_shiftLeftBy_mul =
    -- FULL RANGE FUZZ: shiftLeftBy
    fuzz2 smallInt uint64 "shiftLeftBy n == mul (2^n)" <|
        \shift a ->
            UInt64.shiftLeftBy shift a
                |> Expect.equal
                    (let
                        limitedShift =
                            UInt64.limitSmallInt 6 shift

                        multiplier =
                            if limitedShift < 53 then
                                UInt64.fromInt <| 2 ^ limitedShift

                            else
                                UInt64.mul
                                    (UInt64.fromInt <| 2 ^ (limitedShift - 30))
                                    (UInt64.fromInt <| 2 ^ 30)
                     in
                     UInt64.mul a multiplier
                    )


test_shiftRightZfBy_div =
    -- FULL RANGE FUZZ: shiftRightZfBy
    fuzz2 smallInt uint64 "shiftRightZfBy n == div (2^n)" <|
        \shift a ->
            UInt64.shiftRightZfBy shift a
                |> Expect.equal
                    (let
                        limitedShift =
                            UInt64.limitSmallInt 6 shift

                        divider =
                            if limitedShift < 53 then
                                UInt64.fromInt <| 2 ^ limitedShift

                            else
                                UInt64.mul
                                    (UInt64.fromInt <| 2 ^ (limitedShift - 30))
                                    (UInt64.fromInt <| 2 ^ 30)
                     in
                     Tuple.first <| UInt64.divMod a divider
                    )



-- TESTS - COMPARISON


test_compare =
    describe "compare"
        [ -- INDIVIDUAL
          test "high LT" <|
            \_ -> UInt64.compare (UInt64.fromInt24s 12 13 13) (UInt64.fromInt24s 13 12 12) |> Expect.equal LT
        , test "high GT" <|
            \_ -> UInt64.compare (UInt64.fromInt24s 13 12 12) (UInt64.fromInt24s 12 13 13) |> Expect.equal GT
        , test "mid LT" <|
            \_ -> UInt64.compare (UInt64.fromInt24s 12 12 13) (UInt64.fromInt24s 12 13 12) |> Expect.equal LT
        , test "mid GT" <|
            \_ -> UInt64.compare (UInt64.fromInt24s 12 13 12) (UInt64.fromInt24s 12 12 13) |> Expect.equal GT
        , test "low LT" <|
            \_ -> UInt64.compare (UInt64.fromInt24s 12 12 12) (UInt64.fromInt24s 12 12 13) |> Expect.equal LT
        , test "low GT" <|
            \_ -> UInt64.compare (UInt64.fromInt24s 12 12 13) (UInt64.fromInt24s 12 12 12) |> Expect.equal GT
        , test "EQ" <|
            \_ -> UInt64.compare (UInt64.fromInt24s 12 13 14) (UInt64.fromInt24s 12 13 14) |> Expect.equal EQ

        -- FUZZ
        -- FULL RANGE FUZZ: compare
        , fuzz2 uint64 uint64 "fuzz" <|
            -- generating a,b directly would "never" get equal values, unlike with delta
            \a delta ->
                let
                    b =
                        UInt64.add a delta
                in
                UInt64.compare a b |> Expect.equal (Basics.compare (UInt64.toInt24s a) (UInt64.toInt24s b))
        ]


test_isSafe =
    describe "isSafe"
        [ -- INDIVIDUAL
          test "0x001FFFFFFFFFFFFF --> True" <|
            \_ -> UInt64.fromInt24s 0x1F 0x00FFFFFF 0x00FFFFFF |> UInt64.isSafe |> Expect.true "True"
        , test "0x0020000000000000 --> False" <|
            \_ -> UInt64.fromInt24s 0x20 0 0 |> UInt64.isSafe |> Expect.false "False"

        -- FUZZ
        -- FULL RANGE FUZZ: isSafe
        , fuzz uint64 "fuzz" <|
            \a -> UInt64.isSafe a |> Expect.equal (UInt64.compare a UInt64.maxSafe /= GT)
        ]



-- TESTS - INTERNALS


testInternals =
    describe "internal constants"
        [ test "floatNaN" <|
            \_ -> isNaN floatNaN |> Expect.equal True
        , test "floatNegInf" <|
            \_ -> isInfinite floatNegInf && floatNegInf < 0 |> Expect.equal True
        , test "floatPosInf" <|
            \_ -> isInfinite floatPosInf && floatPosInf > 0 |> Expect.equal True
        ]



-- CONSTANTS


floatNaN : Float
floatNaN =
    0 / 0


floatNegInf : Float
floatNegInf =
    -1 / 0


floatPosInf : Float
floatPosInf =
    1 / 0


limit30 : Int
limit30 =
    0x40000000


{-| Maximum possible non-integer, except `+Infinity`
-}
maxNonInteger : Float
maxNonInteger =
    4503599627370495.5


{-| Minimum value known for certain to be an integer, and so doesn't need to use `Basics.floor`.

`2 ^ 52 = 4503599627370496`

-}
minCertainInteger : Float
minCertainInteger =
    4503599627370496.0



-- FUZZERS


{-| For arguments checked with `limitFloat`.
-}
anyFloat : Float -> Fuzzer Float
anyFloat max =
    Fuzz.frequency
        [ ( 1, Fuzz.constant floatNaN )
        , ( 1, Fuzz.constant floatNegInf )
        , ( 1, Fuzz.constant floatPosInf )

        -- below safe integer range
        , ( 1, Fuzz.constant -9007199254740992.0 )

        -- negative values aren't that interesting here
        , ( 1, floatRange -9007199254740992.0 0.0 )
        , ( 1, Fuzz.constant 0.0 )
        , ( 10, floatRange 0.0 max )
        , ( 1, Fuzz.constant max )

        -- above max values aren't that interesting here
        , ( 1, floatRange max (10.0 * max) )

        -- above safe integer range
        , ( 1, Fuzz.constant 9007199254740992.0 )
        ]


{-| This doesn't seem to cause RangeError unlike `Fuzz.floatRange`.
-}
floatRange : Float -> Float -> Fuzzer Float
floatRange min max =
    Fuzz.custom (Random.float min max) Shrink.noShrink


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


{-| Alternative implementation of `UInt64.fromString` to test against.
-}
alternativeFromString : String -> Maybe UInt64
alternativeFromString str =
    if String.startsWith "0x" str then
        fromUnprefixedString (UInt64.fromInt24s 0 0 16) charToHexDigit <| String.dropLeft 2 str

    else if String.startsWith "0o" str then
        fromUnprefixedString (UInt64.fromInt24s 0 0 8) charToOctalDigit <| String.dropLeft 2 str

    else if String.startsWith "0b" str then
        fromUnprefixedString (UInt64.fromInt24s 0 0 2) charToBinaryDigit <| String.dropLeft 2 str

    else
        fromUnprefixedString (UInt64.fromInt24s 0 0 10) charToDecimalDigit str


charToBinaryDigit =
    Dict.fromList
        [ ( '0', 0 ), ( '1', 1 ) ]


charToDecimalDigit =
    Dict.fromList
        [ ( '0', 0 ), ( '1', 1 ), ( '2', 2 ), ( '3', 3 ), ( '4', 4 ), ( '5', 5 ), ( '6', 6 ), ( '7', 7 ), ( '8', 8 ), ( '9', 9 ) ]


charToHexDigit =
    Dict.fromList <|
        [ ( '0', 0 ), ( '1', 1 ), ( '2', 2 ), ( '3', 3 ), ( '4', 4 ), ( '5', 5 ), ( '6', 6 ), ( '7', 7 ), ( '8', 8 ), ( '9', 9 ) ]
            ++ [ ( 'A', 10 ), ( 'B', 11 ), ( 'C', 12 ), ( 'D', 13 ), ( 'E', 14 ), ( 'F', 15 ) ]
            ++ [ ( 'a', 10 ), ( 'b', 11 ), ( 'c', 12 ), ( 'd', 13 ), ( 'e', 14 ), ( 'f', 15 ) ]


charToOctalDigit =
    Dict.fromList
        [ ( '0', 0 ), ( '1', 1 ), ( '2', 2 ), ( '3', 3 ), ( '4', 4 ), ( '5', 5 ), ( '6', 6 ), ( '7', 7 ) ]


{-| Helper for `alternativeFromString`.
-}
fromUnprefixedString : UInt64 -> Dict Char Int -> String -> Maybe UInt64
fromUnprefixedString base charToDigit str =
    if String.isEmpty str then
        Nothing

    else
        str
            |> String.foldl
                (\char accum ->
                    case ( accum, Dict.get char charToDigit ) of
                        ( Just accum_, Just digitInt ) ->
                            let
                                digit =
                                    UInt64.fromInt digitInt

                                newAccum =
                                    UInt64.add digit <| UInt64.mul accum_ base

                                ( div, mod ) =
                                    UInt64.divMod newAccum base
                            in
                            if div /= accum_ || mod /= digit then
                                -- overflow
                                Nothing

                            else
                                Just newAccum

                        _ ->
                            Nothing
                )
                (Just UInt64.zero)


{-| Different implementation from UInt64.elm to compare against.
-}
nibbleToHex : Int -> Char
nibbleToHex x =
    case x of
        0 ->
            '0'

        1 ->
            '1'

        2 ->
            '2'

        3 ->
            '3'

        4 ->
            '4'

        5 ->
            '5'

        6 ->
            '6'

        7 ->
            '7'

        8 ->
            '8'

        9 ->
            '9'

        10 ->
            'A'

        11 ->
            'B'

        12 ->
            'C'

        13 ->
            'D'

        14 ->
            'E'

        15 ->
            'F'

        _ ->
            -- different character for invalid argument, compated to UInt64.elm
            '*'


{-| This can be replaced with ~~ UInt64.toBinString / UInt64.toOctString ~~ if those are implemented.
-}
toBinaryOrOctalString : UInt64 -> String -> UInt64 -> String
toBinaryOrOctalString divisor tail x =
    let
        ( rest, digit ) =
            UInt64.divMod x divisor

        char =
            case UInt64.toInt31 digit of
                Nothing ->
                    'X'

                Just digit_ ->
                    Char.fromCode <| Char.toCode '0' + digit_
    in
    if rest == UInt64.zero then
        String.cons char tail

    else
        toBinaryOrOctalString divisor (String.cons char tail) rest

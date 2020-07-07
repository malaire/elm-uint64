module DigitsTests exposing (all)

import Expect
import Fuzz exposing (Fuzzer)
import List.Extra
import Test exposing (..)
import TestUtil exposing (smallInt, uint64)
import UInt64 exposing (UInt64)
import UInt64.Digits as Digits
import UInt64.Internal as Internal exposing (Base(..), Digits(..), UpperOrLower(..))


all =
    describe "UInt64.Digits ; toDigits"
        [ -- UINT64
          test_toDigits
        , test_toIntDigits

        -- PADDING
        , test_pad
        , test_padToEven
        , test_padToMultipleOf
        , test_padToPowerOfTwo

        -- MAPPING
        , test_map

        -- CONVERSION - STRING
        , test_groupToString
        , test_toString

        -- CONVERSION - LIST
        , test_groupToFlatList
        , test_groupToList
        , test_toList
        , test_toListWithLength
        ]



-- TESTS - UINT64


test_toDigits =
    let
        zeroTest baseStr base =
            test (baseStr ++ " zero") <|
                \_ -> UInt64.toDigits base UInt64.zero |> Digits.toListWithLength |> Expect.equal ( 1, [ '0' ] )
    in
    describe "UInt64.toDigits"
        [ -- INDIVIDUAL
          zeroTest "decimal" Digits.decimal
        , zeroTest "hex" Digits.hex
        , zeroTest "hexLower" Digits.hexLower
        , zeroTest "hexUpper" Digits.hexUpper
        , zeroTest "octal" Digits.octal
        , zeroTest "binary" Digits.binary
        ]


test_toIntDigits =
    let
        zeroTest baseStr base =
            test (baseStr ++ " zero") <|
                \_ -> UInt64.toIntDigits base UInt64.zero |> Digits.toListWithLength |> Expect.equal ( 1, [ 0 ] )
    in
    describe "UInt64.toIntDigits"
        [ -- INDIVIDUAL
          zeroTest "decimal" Digits.decimal
        , zeroTest "hex" Digits.hex
        , zeroTest "hexLower" Digits.hexLower
        , zeroTest "hexUpper" Digits.hexUpper
        , zeroTest "octal" Digits.octal
        , zeroTest "binary" Digits.binary
        ]



-- TESTS - PADDING


test_pad =
    describe "pad"
        [ -- INDIVIDUAL
          test "0 '.' empty" <|
            \_ ->
                Digits.empty
                    |> Digits.pad 0 '.'
                    |> Digits.toString
                    |> Expect.equal "................................................................"

        -- FUZZ
        -- FULL RANGE FUZZ: pad
        , fuzz3 maybeUInt64 baseFuzzer (Fuzz.tuple ( smallInt, Fuzz.int )) "fuzz" <|
            \a base ( givenPaddedDigitCount, padding ) ->
                let
                    limitedPaddedDigitCount =
                        UInt64.limitSmallInt 6 False givenPaddedDigitCount

                    ( digitCount, digits ) =
                        alternativeToIntDigitsMaybe base a

                    expected =
                        if limitedPaddedDigitCount <= digitCount then
                            Digits ( digitCount, digits )

                        else
                            Digits
                                ( limitedPaddedDigitCount
                                , List.repeat (limitedPaddedDigitCount - digitCount) padding ++ digits
                                )
                in
                a
                    |> Maybe.map (UInt64.toIntDigits base)
                    |> Maybe.withDefault Digits.empty
                    |> Digits.pad givenPaddedDigitCount padding
                    |> Expect.equal expected
        ]


test_padToEven =
    describe "padToEven"
        [ -- FULL RANGE FUZZ: padToEven
          fuzz3 maybeUInt64 baseFuzzer Fuzz.int "fuzz" <|
            \a base padding ->
                let
                    ( digitCount, digits ) =
                        alternativeToIntDigitsMaybe base a

                    expected =
                        if Basics.modBy 2 digitCount == 0 then
                            Digits ( digitCount, digits )

                        else
                            Digits ( digitCount + 1, padding :: digits )
                in
                a
                    |> Maybe.map (UInt64.toIntDigits base)
                    |> Maybe.withDefault Digits.empty
                    |> Digits.padToEven padding
                    |> Expect.equal expected
        ]


test_padToMultipleOf =
    describe "padToMultipleOf"
        [ -- FULL RANGE FUZZ: padToMultipleOf
          fuzz3 maybeUInt64 baseFuzzer (Fuzz.tuple ( smallInt, Fuzz.int )) "fuzz" <|
            \a base ( givenFactor, padding ) ->
                let
                    limitedFactor =
                        UInt64.limitSmallInt 6 False givenFactor

                    ( digitCount, digits ) =
                        alternativeToIntDigitsMaybe base a

                    expected =
                        if Basics.modBy limitedFactor digitCount == 0 then
                            Digits ( digitCount, digits )

                        else
                            let
                                paddedDigitCount =
                                    digitCount - Basics.modBy limitedFactor digitCount + limitedFactor
                            in
                            Digits
                                ( paddedDigitCount
                                , List.repeat (paddedDigitCount - digitCount) padding ++ digits
                                )
                in
                a
                    |> Maybe.map (UInt64.toIntDigits base)
                    |> Maybe.withDefault Digits.empty
                    |> Digits.padToMultipleOf givenFactor padding
                    |> Expect.equal expected
        ]


test_padToPowerOfTwo =
    describe "padToPowerOfTwo"
        [ -- FULL RANGE FUZZ: padToPowerOfTwo
          fuzz3 maybeUInt64 baseFuzzer Fuzz.int "fuzz" <|
            \a base padding ->
                let
                    ( digitCount, digits ) =
                        alternativeToIntDigitsMaybe base a

                    paddedDigitCount =
                        2 ^ (Basics.ceiling <| Basics.logBase 2.0 <| Basics.toFloat digitCount)

                    expected =
                        if paddedDigitCount <= digitCount then
                            Digits ( digitCount, digits )

                        else
                            Digits
                                ( paddedDigitCount
                                , List.repeat (paddedDigitCount - digitCount) padding ++ digits
                                )
                in
                a
                    |> Maybe.map (UInt64.toIntDigits base)
                    |> Maybe.withDefault Digits.empty
                    |> Digits.padToPowerOfTwo padding
                    |> Expect.equal expected
        ]



-- TESTS - MAPPING


test_map =
    describe "map"
        [ -- FULL RANGE FUZZ: map
          fuzz3 maybeUInt64 baseFuzzer Fuzz.int "fuzz" <|
            \a base delta ->
                a
                    |> Maybe.map (UInt64.toIntDigits base)
                    |> Maybe.withDefault Digits.empty
                    |> Digits.map ((+) delta)
                    |> Expect.equal
                        (alternativeToIntDigitsMaybe base a
                            |> Tuple.mapSecond (List.map ((+) delta))
                            |> Digits
                        )
        ]



-- TESTS - CONVERSION - STRING


test_groupToString =
    describe "groupToString"
        [ -- FULL RANGE FUZZ: groupToString
          fuzz3 maybeUInt64 baseFuzzer (Fuzz.tuple ( smallInt, TestUtil.anyChar )) "fuzz" <|
            \a base ( givenGroupSize, separator ) ->
                let
                    limitedGroupSize =
                        UInt64.limitSmallInt 6 False givenGroupSize
                in
                a
                    |> Maybe.map (UInt64.toDigits base)
                    |> Maybe.withDefault Digits.empty
                    |> Digits.groupToString givenGroupSize separator
                    |> Expect.equal
                        (alternativeToDigitsMaybe base a
                            |> Tuple.second
                            |> List.reverse
                            |> List.Extra.greedyGroupsOf limitedGroupSize
                            |> List.intersperse [ separator ]
                            |> List.concat
                            |> List.reverse
                            |> String.fromList
                        )
        ]


test_toString =
    describe "toString"
        [ -- INDIVIDUAL
          test "empty" <|
            \_ -> Digits.empty |> Digits.toString |> Expect.equal ""

        -- FUZZ
        -- FULL RANGE FUZZ: toString  ; augmented by "empty" above
        , fuzz2 uint64 baseFuzzer "fuzz" <|
            \a base ->
                UInt64.toDigits base a
                    |> Digits.toString
                    |> Expect.equal
                        (alternativeToDigits base a
                            |> Tuple.second
                            |> String.fromList
                        )
        ]



-- TESTS - CONVERSION - LIST


test_groupToFlatList =
    describe "groupToFlatList"
        [ -- FULL RANGE FUZZ: groupToFlatList
          fuzz3 maybeUInt64 baseFuzzer (Fuzz.tuple ( smallInt, Fuzz.int )) "fuzz" <|
            \a base ( givenGroupSize, separator ) ->
                let
                    limitedGroupSize =
                        UInt64.limitSmallInt 6 False givenGroupSize
                in
                a
                    |> Maybe.map (UInt64.toIntDigits base)
                    |> Maybe.withDefault Digits.empty
                    |> Digits.groupToFlatList givenGroupSize separator
                    |> Expect.equal
                        (alternativeToIntDigitsMaybe base a
                            |> Tuple.second
                            |> List.reverse
                            |> List.Extra.greedyGroupsOf limitedGroupSize
                            |> List.intersperse [ separator ]
                            |> List.concat
                            |> List.reverse
                        )
        ]


test_groupToList =
    describe "groupToList"
        [ -- FULL RANGE FUZZ: groupToList
          fuzz3 maybeUInt64 baseFuzzer smallInt "fuzz" <|
            \a base givenGroupSize ->
                let
                    limitedGroupSize =
                        UInt64.limitSmallInt 6 False givenGroupSize
                in
                a
                    |> Maybe.map (UInt64.toIntDigits base)
                    |> Maybe.withDefault Digits.empty
                    |> Digits.groupToList givenGroupSize
                    |> Expect.equal
                        (alternativeToIntDigitsMaybe base a
                            |> Tuple.second
                            |> List.reverse
                            |> List.Extra.greedyGroupsOf limitedGroupSize
                            |> List.map List.reverse
                            |> List.reverse
                        )
        ]


test_toList =
    describe "toList"
        [ -- INDIVIDUAL
          test "empty" <|
            \_ -> Digits.empty |> Digits.toList |> Expect.equal []

        -- FUZZ
        -- FULL RANGE FUZZ: toList  ; these two together, augmented by "empty" above
        , fuzz2 uint64 baseFuzzer "fuzz with toDigits" <|
            \a base ->
                UInt64.toDigits base a
                    |> Digits.toList
                    |> Expect.equal (alternativeToDigits base a |> Tuple.second)
        , fuzz2 uint64 baseFuzzer "fuzz with toIntDigits" <|
            \a base ->
                UInt64.toIntDigits base a
                    |> Digits.toList
                    |> Expect.equal (alternativeToIntDigits base a |> Tuple.second)
        ]


test_toListWithLength =
    describe "toListWithLength"
        [ -- INDIVIDUAL
          test "empty" <|
            \_ -> Digits.empty |> Digits.toListWithLength |> Expect.equal ( 0, [] )

        -- FUZZ
        -- FULL RANGE FUZZ: toListWithLength  ; these two together, augmented by "empty" above
        -- FULL RANGE FUZZ: toDigits  ; augmented by test_toDigits
        , fuzz2 uint64 baseFuzzer "fuzz with toDigits" <|
            \a base ->
                UInt64.toDigits base a
                    |> Digits.toListWithLength
                    |> Expect.equal (alternativeToDigits base a)

        -- FULL RANGE FUZZ: toIntDigits  ; augmented by test_toIntDigits
        , fuzz2 uint64 baseFuzzer "fuzz with toIntDigits" <|
            \a base ->
                UInt64.toIntDigits base a
                    |> Digits.toListWithLength
                    |> Expect.equal (alternativeToIntDigits base a)
        ]



-- FUZZERS


baseFuzzer : Fuzzer Base
baseFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant Decimal
        , Fuzz.constant (Hex Upper)
        , Fuzz.constant (Hex Lower)
        , Fuzz.constant Octal
        , Fuzz.constant Binary
        ]


maybeUInt64 : Fuzzer (Maybe UInt64)
maybeUInt64 =
    Fuzz.frequency
        [ ( 1, Fuzz.constant Nothing )
        , ( 9, Fuzz.map Just uint64 )
        ]



-- HELPERS


baseInfo : Base -> ( Int, Bool )
baseInfo base =
    case base of
        Decimal ->
            ( 10, True )

        Hex Upper ->
            ( 16, True )

        Hex Lower ->
            ( 16, False )

        Octal ->
            ( 8, True )

        Binary ->
            ( 2, True )


{-| Alternative implementation of `UInt64.toDigits` to test against.
-}
alternativeToDigits : Base -> UInt64 -> ( Int, List Char )
alternativeToDigits base x =
    let
        ( _, upper ) =
            baseInfo base
    in
    alternativeToIntDigits base x
        |> Tuple.mapSecond (List.map <| TestUtil.riskyIntToCharDigit upper)


alternativeToDigitsMaybe : Base -> Maybe UInt64 -> ( Int, List Char )
alternativeToDigitsMaybe base x =
    case x of
        Just x_ ->
            alternativeToDigits base x_

        Nothing ->
            ( 0, [] )


{-| Alternative implementation of `UInt64.toIntDigits` to test against.
-}
alternativeToIntDigits : Base -> UInt64 -> ( Int, List Int )
alternativeToIntDigits base x =
    let
        ( baseInt, _ ) =
            baseInfo base
    in
    if UInt64.isZero x then
        ( 1, [ 0 ] )

    else
        alternativeToIntDigits_helper (UInt64.fromInt baseInt) x []
            |> (\list -> ( List.length list, list ))


alternativeToIntDigits_helper : UInt64 -> UInt64 -> List Int -> List Int
alternativeToIntDigits_helper base64 x digits =
    if UInt64.isZero x then
        digits

    else
        let
            ( rest, digit ) =
                UInt64.divMod x base64

            digitInt =
                UInt64.toInt53 digit |> Maybe.withDefault 0
        in
        alternativeToIntDigits_helper base64 rest (digitInt :: digits)


alternativeToIntDigitsMaybe : Base -> Maybe UInt64 -> ( Int, List Int )
alternativeToIntDigitsMaybe base x =
    case x of
        Just x_ ->
            alternativeToIntDigits base x_

        Nothing ->
            ( 0, [] )

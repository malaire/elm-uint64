module UInt64.Digits exposing
    ( Digits
    , empty
    , Base, decimal, hex, octal, binary, hexUpper, hexLower
    , pad, padToEven, padToMultipleOf, padToPowerOfTwo
    , map
    , toString, groupToString
    , toList, toListWithLength, groupToFlatList, groupToList
    )

{-| Working with list of digits, e.g. padding digits and grouping digits.

    import UInt64
    import UInt64.Digits as Digits

    UInt64.fromInt 7654321
        |> UInt64.toDigits Digits.decimal
        |> Digits.groupToString 3 ' '
        --> "7 654 321"

    UInt64.fromInt 0xABCDE
        |> UInt64.toDigits Digits.hexLower
        |> Digits.padToPowerOfTwo '0'
        |> Digits.toString
        |> (++) "0x"
        --> "0x000abcde"

    UInt64.fromInt 0x10F
        |> UInt64.toDigits Digits.binary
        |> Digits.padToMultipleOf 4 '0'
        |> Digits.pad 32 '.'
        |> Digits.groupToString 4 ' '
        --> ".... .... .... .... .... 0001 0000 1111"

    UInt64.fromInt 0x1FF
        |> UInt64.toIntDigits Digits.octal
        |> Digits.toList
        --> [ 7, 7, 7 ]


# Contents

  - [Type](#type)
      - [`Digits`](#Digits)
  - [Create](#create)
      - [`empty`](#empty)
  - [Bases](#bases)
      - [`Base`](#Base)
      - [`decimal`](#decimal), [`hex`](#hex), [`octal`](#octal), [`binary`](#binary)
      - [`hexUpper`](#hexUpper), [`hexLower`](#hexLower)
  - [Padding](#padding)
      - [`pad`](#pad), [`padToEven`](#padToEven), [`padToMultipleOf`](#padToMultipleOf), [`padToPowerOfTwo`](#padToPowerOfTwo)
  - [Mapping](#Mapping)
      - [`map`](#map)
  - [Conversion - String](#conversion-string)
      - [`toString`](#toString), [`groupToString`](#groupToString)
  - [Conversion - List](#conversion-list)
      - [`toList`](#toList), [`toListWithLength`](#toListWithLength)
      - [`groupToFlatList`](#groupToFlatList), [`groupToList`](#groupToList)


# Type

@docs Digits


# Create

See also [`UInt64.toDigits`](UInt64#toDigits) and [`UInt64.toIntDigits`](UInt64#toIntDigits).

@docs empty


# Bases

@docs Base, decimal, hex, octal, binary, hexUpper, hexLower


# Padding

The padding value given to these functions is not checked or validated in any way.
It can be any value and doesn't need to be a valid digit.

@docs pad, padToEven, padToMultipleOf, padToPowerOfTwo


# Mapping

@docs map


# Conversion - String

@docs toString, groupToString


# Conversion - List

@docs toList, toListWithLength, groupToFlatList, groupToList

-}

import Bitwise
import UInt64.Internal as Internal exposing (Base(..), Digits(..), UpperOrLower(..))



-- TYPE


{-| List of digits.
-}
type alias Digits a =
    Internal.Digits a



-- CREATE


{-| Empty [`Digits`](#Digits).

This can be useful e.g. when converting `Maybe UInt64` to [`Digits`](#Digits),
so you can use [`empty`](#empty) for `Nothing`.

    import UInt64.Digits as Digits

    Digits.empty
        |> Digits.toList
        --> []

-}
empty : Digits a
empty =
    Digits ( 0, [] )



-- BASES


{-| Base used with [`toDigits`](UInt64#toDigits) and [`toIntDigits`](UInt64#toIntDigits)
to convert [`UInt64`](UInt64#UInt64) to [`Digits`](#Digits).
-}
type alias Base =
    Internal.Base


{-| Decimal aka base 10.

  - Digits used with [`toDigits`](UInt64#toDigits): `0123456789`

-}
decimal : Base
decimal =
    Decimal


{-| Hexadecimal aka base 16, using uppercase characters.

  - Digits used with [`toDigits`](UInt64#toDigits): `0123456789ABCDEF`

-}
hex : Base
hex =
    Hex Upper


{-| Hexadecimal aka base 16, using lowercase characters.

  - Digits used with [`toDigits`](UInt64#toDigits): `0123456789abcdef`

-}
hexLower : Base
hexLower =
    Hex Lower


{-| Hexadecimal aka base 16, using uppercase characters.

This is same as [`hex`](#hex).

  - Digits used with [`toDigits`](UInt64#toDigits): `0123456789ABCDEF`

-}
hexUpper : Base
hexUpper =
    Hex Upper


{-| Octal aka base 8.

  - Digits used with [`toDigits`](UInt64#toDigits): `01234567`

-}
octal : Base
octal =
    Octal


{-| Binary aka base 2.

  - Digits used with [`toDigits`](UInt64#toDigits): `01`

-}
binary : Base
binary =
    Binary



-- PADDING


{-| Pad to given number of digits.

  - `Int`: `1 <= x <= 64`

See [argument handling](UInt64#argument-handling).

    import UInt64
    import UInt64.Digits as Digits

    UInt64.fromInt 271
        |> UInt64.toDigits Digits.binary
        |> Digits.pad 32 '0'
        |> Digits.toString
        --> "00000000000000000000000100001111"

-}
pad : Int -> a -> Digits a -> Digits a
pad givenPaddedDigitCount padding ((Digits ( digitCount, digits )) as x) =
    let
        limitedPaddedDigitCount =
            Bitwise.and 0x3F givenPaddedDigitCount

        paddedDigitCount =
            if limitedPaddedDigitCount == 0 then
                64

            else
                limitedPaddedDigitCount
    in
    if paddedDigitCount > digitCount then
        Digits
            ( paddedDigitCount
            , List.repeat (paddedDigitCount - digitCount) padding ++ digits
            )

    else
        x


{-| Pad to even number of digits.

    import UInt64
    import UInt64.Digits as Digits

    UInt64.fromInt 271
        |> UInt64.toDigits Digits.binary
        |> Digits.padToEven '0'
        |> Digits.toString
        --> "0100001111"

-}
padToEven : a -> Digits a -> Digits a
padToEven padding ((Digits ( digitCount, digits )) as x) =
    if Bitwise.and 0x01 digitCount == 1 then
        Digits ( digitCount + 1, padding :: digits )

    else
        x


{-| Pad to multiple of given number of digits.

For example `padToMultipleOf 16` would pad to 0, 16, 32, 48 or 64 digits.

  - `Int`: `1 <= x <= 64`

See [argument handling](UInt64#argument-handling).

    import UInt64
    import UInt64.Digits as Digits

    UInt64.fromInt 271
        |> UInt64.toDigits Digits.binary
        |> Digits.padToMultipleOf 4 '0'
        |> Digits.toString
        --> "000100001111"

-}
padToMultipleOf : Int -> a -> Digits a -> Digits a
padToMultipleOf givenFactor padding ((Digits ( digitCount, digits )) as x) =
    let
        limitedFactor =
            Bitwise.and 0x3F givenFactor

        factor =
            if limitedFactor == 0 then
                64

            else
                limitedFactor

        paddedDigitCount =
            (digitCount + factor - 1) // factor * factor
    in
    if paddedDigitCount > digitCount then
        Digits
            ( paddedDigitCount
            , List.repeat (paddedDigitCount - digitCount) padding ++ digits
            )

    else
        x


{-| Pad to `2^n` digits, i.e. to 0, 1, 2, 4, 8, 16, 32 or 64 digits.

    import UInt64
    import UInt64.Digits as Digits

    UInt64.fromInt 271
        |> UInt64.toDigits Digits.binary
        |> Digits.padToPowerOfTwo '0'
        |> Digits.toString
        --> "0000000100001111"

-}
padToPowerOfTwo : a -> Digits a -> Digits a
padToPowerOfTwo padding ((Digits ( digitCount, digits )) as x) =
    let
        -- This algorithm is from
        --   https://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
        c0 =
            digitCount - 1

        c1 =
            Bitwise.or c0 (Bitwise.shiftRightZfBy 1 c0)

        c2 =
            Bitwise.or c1 (Bitwise.shiftRightZfBy 2 c1)

        c3 =
            Bitwise.or c2 (Bitwise.shiftRightZfBy 4 c2)

        paddedDigitCount =
            c3 + 1
    in
    if paddedDigitCount > digitCount then
        Digits
            ( paddedDigitCount
            , List.repeat (paddedDigitCount - digitCount) padding ++ digits
            )

    else
        x



-- MAPPING


{-| Apply a function to every digit.

If [`Digits`](#Digits) has been padded, then function is also applied to added padding, if any.

    import UInt64
    import UInt64.Digits as Digits

    UInt64.fromInt 11
        |> UInt64.toIntDigits Digits.binary
        |> Digits.map ((==) 1)
        |> Digits.toList
        --> [ True, False, True, True ]

-}
map : (a -> b) -> Digits a -> Digits b
map fn (Digits ( digitCount, digits )) =
    Digits ( digitCount, List.map fn digits )



-- CONVERSION - STRING


{-| Convert [`Digits`](#Digits) of `Char` to `String` of grouped digits.

Groups are separated by given `separator`.

  - `groupSize`: `1 <= x <= 64`

See [argument handling](UInt64#argument-handling).

    import UInt64
    import UInt64.Digits as Digits

    UInt64.fromInt 7654321
        |> UInt64.toDigits Digits.decimal
        |> Digits.groupToString 3 ' '
        --> "7 654 321"

-}
groupToString : Int -> Char -> Digits Char -> String
groupToString givenGroupSize separator digits =
    groupToFlatList givenGroupSize separator digits
        |> String.fromList


{-| Convert [`Digits`](#Digits) of `Char` to `String`.

    import UInt64
    import UInt64.Digits as Digits

    UInt64.fromInt 7654321
        |> UInt64.toDigits Digits.decimal
        |> Digits.toString
        --> "7654321"

-}
toString : Digits Char -> String
toString (Digits ( _, digits )) =
    String.fromList digits



-- CONVERSION - LIST


{-| Convert [`Digits`](#Digits) to flat `List` of grouped digits.

Groups are separated by given `separator`.

  - `groupSize`: `1 <= x <= 64`

See [argument handling](UInt64#argument-handling).

    import UInt64
    import UInt64.Digits as Digits

    UInt64.fromInt 7654321
        |> UInt64.toDigits Digits.decimal
        |> Digits.groupToFlatList 3 ' '
        --> [ '7', ' ', '6', '5', '4', ' ', '3', '2', '1' ]

-}
groupToFlatList : Int -> a -> Digits a -> List a
groupToFlatList givenGroupSize separator (Digits ( digitCount, digits )) =
    let
        limitedGroupSize =
            Bitwise.and 0x3F givenGroupSize

        groupSize =
            if limitedGroupSize == 0 then
                64

            else
                limitedGroupSize

        firstGroupSize_ =
            Basics.modBy groupSize digitCount

        firstGroupSize =
            if firstGroupSize_ == 0 then
                groupSize

            else
                firstGroupSize_
    in
    groupToFlatListHelper groupSize separator firstGroupSize digits


groupToFlatListHelper : Int -> a -> Int -> List a -> List a
groupToFlatListHelper groupSize separator groupLeft digits =
    case digits of
        [] ->
            []

        x :: xs ->
            if groupLeft == 0 then
                separator :: x :: groupToFlatListHelper groupSize separator (groupSize - 1) xs

            else
                x :: groupToFlatListHelper groupSize separator (groupLeft - 1) xs


{-| Convert [`Digits`](#Digits) to `List` of grouped digits.

Each group will be a separate sub-`List`.

  - `groupSize`: `1 <= x <= 64`

See [argument handling](UInt64#argument-handling).

    import UInt64
    import UInt64.Digits as Digits

    UInt64.fromInt 7654321
        |> UInt64.toDigits Digits.decimal
        |> Digits.groupToList 3
        --> [ [ '7' ], [ '6', '5', '4' ], [ '3', '2', '1' ] ]

-}
groupToList : Int -> Digits a -> List (List a)
groupToList givenGroupSize (Digits ( digitCount, digits )) =
    if digitCount == 0 then
        []

    else
        let
            limitedGroupSize =
                Bitwise.and 0x3F givenGroupSize

            groupSize =
                if limitedGroupSize == 0 then
                    64

                else
                    limitedGroupSize

            firstGroupSize_ =
                Basics.modBy groupSize digitCount

            firstGroupSize =
                if firstGroupSize_ == 0 then
                    groupSize

                else
                    firstGroupSize_
        in
        List.take firstGroupSize digits
            :: groupToListHelper groupSize (List.drop firstGroupSize digits)


groupToListHelper : Int -> List a -> List (List a)
groupToListHelper groupSize digits =
    case digits of
        [] ->
            []

        _ ->
            List.take groupSize digits
                :: groupToListHelper groupSize (List.drop groupSize digits)


{-| Convert [`Digits`](#Digits) to `List` of digits.

    import UInt64
    import UInt64.Digits as Digits

    UInt64.fromInt 7654321
        |> UInt64.toDigits Digits.decimal
        |> Digits.toList
        --> [ '7', '6', '5', '4', '3', '2', '1' ]

-}
toList : Digits a -> List a
toList (Digits ( _, digits )) =
    digits


{-| Convert [`Digits`](#Digits) to `List` of digits with list length.

This is O(1) while `List.length` is O(n), so if you need list length then
using this is faster than [`toList`](#toList) followed by `List.length`.

    import UInt64
    import UInt64.Digits as Digits

    UInt64.fromInt 7654321
        |> UInt64.toDigits Digits.decimal
        |> Digits.toListWithLength
        --> ( 7, [ '7', '6', '5', '4', '3', '2', '1' ] )

-}
toListWithLength : Digits a -> ( Int, List a )
toListWithLength (Digits tuple) =
    tuple

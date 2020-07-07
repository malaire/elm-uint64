module UInt64 exposing
    ( UInt64
    , minValue, maxValue, maxSafe, maxSafeAsFloat, maxFloat, maxFloatAsFloat, zero, one, two
    , limitSmallInt, limitLargeInt, limitFloat
    , fromInt, toInt31, toInt53
    , floor, toFloat
    , fromInt32s, toInt32s, fromInt24s, toInt24s, fromDecimal12s, fromBigEndianBytes, toBigEndianBytes
    , fromString, toString, toHexString
    , toDigits, toIntDigits
    , add, sub, mul, pow, increment, decrement, square
    , div, mod, divMod
    , and, or, xor, complement, shiftLeftBy, shiftRightZfBy, rotateLeftBy, rotateRightBy, shiftRightZfBy1, getBit, setBit
    , compare
    , isSafe, isZero, isEven, isOdd
    , divModFast, divModSlow
    )

{-| 64-bit unsigned integer using wrapping overflow.

  - [Type](#type)
      - [`UInt64`](#UInt64)
  - [Constants](#constants)
      - [`minValue`](#minValue), [`maxValue`](#maxValue)
      - [`maxSafe`](#maxSafe), [`maxSafeAsFloat`](#maxSafeAsFloat), [`maxFloat`](#maxFloat), [`maxFloatAsFloat`](#maxFloatAsFloat)
      - [`zero`](#zero), [`one`](#one), [`two`](#two)
  - [Argument handling](#argument-handling)
      - [`limitSmallInt`](#limitSmallInt), [`limitLargeInt`](#limitLargeInt), [`limitFloat`](#limitFloat)
  - [Conversion - Int](#conversion-int)
      - [`fromInt`](#fromInt), [`toInt31`](#toInt31), [`toInt53`](#toInt53)
  - [Conversion - Float](#conversion-float)
      - [`floor`](#floor), [`toFloat`](#toFloat)
  - [Conversion - Parts](#conversion-parts)
      - [`fromInt32s`](#fromInt32s), [`toInt32s`](#toInt32s), [`fromInt24s`](#fromInt24s), [`toInt24s`](#toInt24s)
      - [`fromDecimal12s`](#fromDecimal12s)
      - [`fromBigEndianBytes`](#fromBigEndianBytes), [`toBigEndianBytes`](#toBigEndianBytes)
  - [Conversion - String](#conversion-string)
      - [`fromString`](#fromString), [`toString`](#toString), [`toHexString`](#toHexString)
  - [Conversion - Digits](#conversion-digits)
      - [`toDigits`](#toDigits), [`toIntDigits`](#toIntDigits)
  - [Math](#math)
      - [`add`](#add), [`sub`](#sub), [`mul`](#mul), [`pow`](#pow)
      - [`increment`](#increment), [`decrement`](#decrement), [`square`](#square)
  - [Division](#division)
      - [`div`](#div), [`mod`](#mod), [`divMod`](#divMod)
  - [Bitwise](#bitwise)
      - [`and`](#and), [`or`](#or), [`xor`](#xor), [`complement`](#complement)
      - [`shiftLeftBy`](#shiftLeftBy), [`shiftRightZfBy`](#shiftRightZfBy),
        [`rotateLeftBy`](#rotateLeftBy), [`rotateRightBy`](#rotateRightBy)
      - [`shiftRightZfBy1`](#shiftRightZfBy1)
      - [`getBit`](#getBit), [`setBit`](#setBit)
  - [Comparison](#comparison)
      - [`compare`](#compare)
  - [Check](#check)
      - [`isSafe`](#isSafe), [`isZero`](#isZero), [`isEven`](#isEven), [`isOdd`](#isOdd)
  - [Extra](#extra)
      - [`divModFast`](#divModFast), [`divModSlow`](#divModSlow)


# Type

@docs UInt64


# Constants

@docs minValue, maxValue, maxSafe, maxSafeAsFloat, maxFloat, maxFloatAsFloat, zero, one, two


# Argument handling

Every `Int` or `Float` argument of every [`UInt64`](#UInt64) function is limited
by one of the following three functions (or equivalent code):

  - If argument is `Int` and valid value is `0 <= x < 2^n`, `n <= 32`
      - argument is limited by [`limitSmallInt`](#limitSmallInt)`n True value`.
  - If argument is `Int` and valid value is `1 <= x <= 2^n`, `n <= 32`
      - argument is limited by [`limitSmallInt`](#limitSmallInt)`n False value`.
  - If argument is `Int` and valid value can be above `2^32`
      - argument is limited by [`limitLargeInt`](#limitLargeInt)`value`.
  - If argument is `Float` and valid value is `0 <= x <= max`
      - argument is limited by [`limitFloat`](#limitFloat)`max value`.


## Non-integer `Int`

`Int` arguments are expected to be integers
and are not checked for non-integer values like `NaN`, `-Infinity`, `+Infinity` or `12.34`.

**Behavior of any [`UInt64`](#UInt64) function is undefined if `Int` argument is not an integer.**


## Large `Int`

Behavior of `Int` for values above `2^31 - 1` is undefined in Elm.
As of Elm 0.19.1, using `Int` as function parameter or return value works for full safe integer range
up to [`maxSafe`](#maxSafe), but this could change in the future.

**Note:** This affects also [`limitSmallInt`](#limitSmallInt) with `n = 32`.


## `String`

`String` argument can be any valid Unicode `String`.

Behavior is undefined if `String` argument contains invalid Unicode.
Such `String`:s can't be fuzz-tested with `elm-test`,
so I can't make functions robust against invalid Unicode.

@docs limitSmallInt, limitLargeInt, limitFloat


# Conversion - Int

@docs fromInt, toInt31, toInt53


# Conversion - Float

@docs floor, toFloat


# Conversion - Parts

@docs fromInt32s, toInt32s, fromInt24s, toInt24s, fromDecimal12s, fromBigEndianBytes, toBigEndianBytes


# Conversion - String

@docs fromString, toString, toHexString


# Conversion - Digits

These functions convert [`UInt64`](#UInt64) to [`Digits`](UInt64.Digits#Digits),
which offers options like different bases, digits padding and digits grouping.

@docs toDigits, toIntDigits


# Math

@docs add, sub, mul, pow, increment, decrement, square


# Division

**Note:** I would prefer to cause runtime exception on division-by-zero,
but that can't be tested, so I'll settle for returning [`zero`](#zero) which can be tested.

@docs div, mod, divMod


# Bitwise

@docs and, or, xor, complement, shiftLeftBy, shiftRightZfBy, rotateLeftBy, rotateRightBy, shiftRightZfBy1, getBit, setBit


# Comparison

@docs compare


# Check

@docs isSafe, isZero, isEven, isOdd


# Extra

Extra functions which can be useful for some use cases, e.g. benchmarking and testing.

These are not intended to be useful generally.

@docs divModFast, divModSlow

-}

import Bitwise
import UInt64.Digits as Digits
import UInt64.Internal as Internal exposing (Base(..), Digits(..))


{-| 64-bit unsigned integer.

[`UInt64`](#UInt64) is represented internally as three unsigned integers:

  - `high`: 16-bit unsigned integer for bits 48 - 63
  - `mid`: 24-bit unsigned integer for bits 24 - 47
  - `low`: 24-bit unsigned integer for bits 0 - 23

-}
type UInt64
    = UInt64 ( UInt16, UInt24, UInt24 )



-- CONSTANTS


{-| Maximum [`UInt64`](#UInt64) value that can be represented exactly as a `Float`.

`2^64 - 2048 = 18446744073709549568 = 0xFFFFFFFFFFFFF800`

**Note:** `Float` can't represent exactly all integers above [`maxSafe`](#maxSafe).
For example integers [`maxFloat`](#maxFloat)`+ 1 <= x <=`[`maxValue`](#maxValue) can't be represented exactly as `Float`,
making this the maximum [`UInt64`](#UInt64) value that can.

-}
maxFloat : UInt64
maxFloat =
    UInt64 ( max16, max24, 0x00FFF800 )


{-| [`maxFloat`](#maxFloat) as `Float`
-}
maxFloatAsFloat : Float
maxFloatAsFloat =
    -- NOTE: `elm-format` mangles this value.
    -- See https://github.com/avh4/elm-format/issues/680
    18446744073709550000.0


{-| Maximum safe integer as [`UInt64`](#UInt64).

`2^53 - 1 = 9007199254740991 = 0x001FFFFFFFFFFFFF`

Equal to `Number.MAX_SAFE_INTEGER` in JavaScript.

See also [`isSafe`](#isSafe).

-}
maxSafe : UInt64
maxSafe =
    UInt64 ( maxSafeHighPart, max24, max24 )


{-| [`maxSafe`](#maxSafe) as `Float`
-}
maxSafeAsFloat : Float
maxSafeAsFloat =
    9007199254740991.0


{-| Maximum possible [`UInt64`](#UInt64) value.

`2^64 - 1 = 18446744073709551615 = 0xFFFFFFFFFFFFFFFF`

    UInt64.maxValue
        |> UInt64.toString
        --> "18446744073709551615"

-}
maxValue : UInt64
maxValue =
    UInt64 ( max16, max24, max24 )


{-| Minimum possible [`UInt64`](#UInt64) value.

Same as [`zero`](#zero).

    UInt64.minValue
        |> UInt64.toString
        --> "0"

-}
minValue : UInt64
minValue =
    UInt64 ( 0, 0, 0 )


{-| Number `0`
-}
zero : UInt64
zero =
    UInt64 ( 0, 0, 0 )


{-| Number `1`
-}
one : UInt64
one =
    UInt64 ( 0, 0, 1 )


{-| Number `2`
-}
two : UInt64
two =
    UInt64 ( 0, 0, 2 )



-- ARGUMENT HANDLING


{-| Limit `Float` to `0 <= value <= max`.

  - `max`: maximum allowed value, `max <=`[`maxFloat`](#maxFloat)
  - `value`: value to limit

See [argument handling](#argument-handling).

Algorithm:

  - If value is `NaN`, use `0` instead.
  - If value is negative, use `0` instead.
  - If value is above `max`, use `max` instead.
  - If value is not an integer, round down to integer.


## Examples

    -- `-1` limited to 12 decimal digits
    UInt64.limitFloat 999999999999 -1
        --> 0

    -- `1e20` limited to 12 decimal digits
    UInt64.limitFloat 999999999999 1e20
        --> 999999999999

    -- `1e20` limited to `0 <= value <= maxFloat`
    UInt64.limitFloat UInt64.maxFloatAsFloat 1.0e20
        --> 18446744073709549568.0

-}
limitFloat : Float -> Float -> Float
limitFloat givenMax value =
    let
        limitedMax =
            if Basics.isNaN givenMax || givenMax < 0 then
                0

            else if givenMax > maxFloatAsFloat then
                maxFloatAsFloat

            else
                floorAnyPositiveFloat givenMax
    in
    if Basics.isNaN value || value < 0 then
        0

    else if value > limitedMax then
        limitedMax

    else
        floorAnyPositiveFloat value


{-| Limit `Int` to `0 <= value <=`[`maxSafe`](#maxSafe).

See [argument handling](#argument-handling).

Algorithm:

  - If value is negative, use `0` instead.
  - If value is above [`maxSafe`](#maxSafe), use [`maxSafe`](#maxSafe) instead.


## Examples

    -- negative value is replaced with zero
    UInt64.limitLargeInt -1
        --> 0

    -- value above `maxSafe` is replaced with `maxSafe`
    UInt64.limitLargeInt 9007199254740992
        --> 9007199254740991

-}
limitLargeInt : Int -> Int
limitLargeInt value =
    if value < 0 then
        0

    else if value > maxSafeInt then
        maxSafeInt

    else
        value


{-| Limit `Int` to `0 <= x < 2 ^ bitSize` or `1 <= x <= 2 ^ bitSize`.

  - `bitSize`: `1 <= bitSize <= 32`
  - `startFromZero`
      - `True` if valid range is `0 <= x < 2 ^ bitSize`
      - `False` if valid range is `1 <= x <= 2 ^ bitSize`
  - `value`: value to limit

See [argument handling](#argument-handling).

Algorithm:

1.  If value is negative, convert it to positive by two's complement.
2.  Apply unsigned bitwise AND with bitmask `2 ^ bitSize - 1`.
3.  If value is zero and `not startFromZero`, use `2 ^ bitSize` as value.


## Examples

    -- `1234` limited to 6 bits, `0 <= x <= 63`
    UInt64.limitSmallInt 6 True 1234
        --> 18

    -- `-1` limited to 8 bits, `0 <= x <= 255`
    UInt64.limitSmallInt 8 True -1
        --> 0xFF

    -- `-1` limited to 32 bits, `0 <= x <= 2^32-1`
    UInt64.limitSmallInt 32 True -1
        --> 0xFFFFFFFF

    -- `0` limited to 6 bits, `1 <= x <= 64`
    UInt64.limitSmallInt 6 False 0
        --> 64

-}
limitSmallInt : Int -> Bool -> Int -> Int
limitSmallInt givenBitSize startFromZero value =
    let
        maskedBitSize =
            Bitwise.and 0x1F givenBitSize

        limitedBitSize =
            if maskedBitSize == 0 then
                32

            else
                maskedBitSize

        maskedValue =
            if limitedBitSize == 32 then
                Bitwise.shiftRightZfBy 0 value

            else
                Bitwise.and (Bitwise.shiftLeftBy limitedBitSize 1 - 1) value
    in
    if not startFromZero && maskedValue == 0 then
        2 ^ limitedBitSize

    else
        maskedValue



-- CONVERSION - INT


{-| Convert `Int` to [`UInt64`](#UInt64).

  - `value`: `0 <= x <=`[`maxSafe`](#maxSafe)

See [argument handling](#argument-handling).

    UInt64.fromInt 123
        |> UInt64.toString
        --> "123"

-}
fromInt : Int -> UInt64
fromInt x =
    if x <= 0 then
        zero

    else if x <= maxSafeInt then
        let
            high =
                Basics.floor <| Basics.toFloat x / limit48

            midLow =
                x - limit48 * high

            mid =
                Basics.floor <| Basics.toFloat midLow / limit24

            low =
                midLow - limit24 * mid
        in
        UInt64 ( high, mid, low )

    else
        maxSafe


{-| Convert [`UInt64`](#UInt64) to 31-bit unsigned integer.

If [`UInt64`](#UInt64) is above `2^31 - 1`, return `Nothing`.

    UInt64.fromInt 0x7FFFFFFF
        |> UInt64.toInt31
        --> Just 0x7FFFFFFF

    UInt64.fromInt 0x80000000
        |> UInt64.toInt31
        --> Nothing

-}
toInt31 : UInt64 -> Maybe Int
toInt31 (UInt64 ( high, mid, low )) =
    if mid <= 0x7F && high == 0 then
        Just <| Bitwise.or (Bitwise.shiftLeftBy 24 mid) low

    else
        Nothing


{-| Convert [`UInt64`](#UInt64) to 53-bit unsigned integer.

If [`UInt64`](#UInt64) is above [`maxSafe`](#maxSafe), return `Nothing`.

See [large `Int`](#large-int-) note.

    UInt64.maxSafe
        |> UInt64.toInt53
        --> Just 9007199254740991

    UInt64.maxSafe
        |> UInt64.increment
        |> UInt64.toInt53
        --> Nothing

-}
toInt53 : UInt64 -> Maybe Int
toInt53 (UInt64 ( high, mid, low )) =
    if high <= maxSafeHighPart then
        Just <| (high * limit24 + mid) * limit24 + low

    else
        Nothing



-- CONVERSION - FLOAT


{-| Convert `Float` to [`UInt64`](#UInt64), rounding down.

  - `value`: `0 <= x <=`[`maxFloat`](#maxFloat)\*

\*) If `value` is above [`maxValue`](#maxValue), return [`maxValue`](#maxValue).

See [argument handling](#argument-handling).


## Values above maxSafe

Conversion is exact for all possible `Float` integer values from `0` to [`maxFloat`](#maxFloat).
However because `Float` can't represent all integers above [`maxSafe`](#maxSafe),
it can sometimes seem like there is an error.

In the following example value `11222333444555666777.0` can't be represented exactly as a `Float`.
Nearest value that can be represented exactly as a `Float` is `11222333444555667456.0`,
and that is what `UInt64.floor` gets as its argument.
This argument is then converted exactly to [`UInt64`](#UInt64).

    UInt64.floor 11222333444555666777.0
        |> UInt64.toString
        --> "11222333444555667456"

[`fromDecimal12s`](#fromDecimal12s) can be used instead
to convert decimal literals above [`maxSafe`](#maxSafe) to [`UInt64`](#UInt64):

    UInt64.fromDecimal12s 11222333 444555666777
        |> UInt64.toString
        --> "11222333444555666777"

-}
floor : Float -> UInt64
floor x =
    if Basics.isNaN x || x <= 0 then
        zero

    else if x <= maxFloatAsFloat then
        -- !!! x can be outside safe range !!!
        let
            floored =
                if x >= minCertainInteger then
                    x

                else
                    Basics.toFloat <| Basics.floor <| x

            high =
                Basics.floor <| floored / limit48

            midLow =
                floored - limit48 * Basics.toFloat high

            mid =
                Basics.floor <| midLow / limit24

            low =
                midLow - limit24 * Basics.toFloat mid
        in
        UInt64 ( high, mid, Basics.floor low )

    else
        maxValue


{-| Convert [`UInt64`](#UInt64) to `Float`.

Conversion is exact for any value from `0` to [`maxSafe`](#maxSafe),
but above [`maxSafe`](#maxSafe) value is rounded if it can't be represented exactly as `Float`.


## Example

`9007199254740993` can't be represented exactly as `Float`,
so it's rounded to `9007199254740992`.

    UInt64.fromDecimal12s 9007 199254740993
        |> UInt64.toFloat
        --> 9007199254740992

-}
toFloat : UInt64 -> Float
toFloat (UInt64 ( high, mid, low )) =
    (Basics.toFloat high * limit24 + Basics.toFloat mid) * limit24 + Basics.toFloat low



-- CONVERSION - PARTS


{-| Convert list of bytes in big-endian order to [`UInt64`](#UInt64).

  - each `byte`: `0 <= x <= 255`
  - If list has less than 8 bytes, highest bytes will be zeroes.
  - If list has more than 8 bytes, highest extra bytes are ignored.

See [argument handling](#argument-handling).

    UInt64.fromBigEndianBytes [ 0xAB, 0, 0xCD ]
        |> UInt64.toHexString
        --> "0000000000AB00CD"

    List.range 0x01 0x0F
        |> UInt64.fromBigEndianBytes
        |> UInt64.toHexString
        --> "08090A0B0C0D0E0F"

-}
fromBigEndianBytes : List Int -> UInt64
fromBigEndianBytes bytes =
    let
        to16 x y =
            (Bitwise.shiftLeftBy 8 <| Bitwise.and 0xFF x)
                + Bitwise.and 0xFF y

        to24 x y z =
            (Bitwise.shiftLeftBy 16 <| Bitwise.and 0xFF x)
                + (Bitwise.shiftLeftBy 8 <| Bitwise.and 0xFF y)
                + Bitwise.and 0xFF z
    in
    case bytes of
        [] ->
            zero

        [ b0 ] ->
            UInt64 ( 0, 0, to24 0 0 b0 )

        [ b1, b0 ] ->
            UInt64 ( 0, 0, to24 0 b1 b0 )

        [ b2, b1, b0 ] ->
            UInt64 ( 0, 0, to24 b2 b1 b0 )

        [ b3, b2, b1, b0 ] ->
            UInt64 ( 0, to24 0 0 b3, to24 b2 b1 b0 )

        [ b4, b3, b2, b1, b0 ] ->
            UInt64 ( 0, to24 0 b4 b3, to24 b2 b1 b0 )

        [ b5, b4, b3, b2, b1, b0 ] ->
            UInt64 ( 0, to24 b5 b4 b3, to24 b2 b1 b0 )

        [ b6, b5, b4, b3, b2, b1, b0 ] ->
            UInt64 ( to16 0 b6, to24 b5 b4 b3, to24 b2 b1 b0 )

        [ b7, b6, b5, b4, b3, b2, b1, b0 ] ->
            UInt64 ( to16 b7 b6, to24 b5 b4 b3, to24 b2 b1 b0 )

        tooManyBytes ->
            fromBigEndianBytes <| List.drop (List.length tooManyBytes - 8) tooManyBytes


{-| Convert 64-bit unsigned integer represented as two 12-decimal-digit unsigned integers to [`UInt64`](#UInt64).

  - `high`: 8 highest digits, `0 <= high <= 18446745`\*
  - `low`: 12 lowest digits, `0 <= low <= 999999999999`

\*) If `high` is `18446745`, return [`maxValue`](#maxValue).

See [argument handling](#argument-handling).

    UInt64.fromDecimal12s 111222 333444555666
        |> UInt64.toString
        --> "111222333444555666"

    UInt64.fromDecimal12s 1 2
        |> UInt64.toString
        --> "1000000000002"

-}
fromDecimal12s : Float -> Float -> UInt64
fromDecimal12s givenHigh givenLow =
    let
        limitedHigh =
            limitFloat 18446745.0 givenHigh

        limitedLow =
            limitFloat 999999999999.0 givenLow
    in
    -- maxValue == 18446744|073709551615
    if limitedHigh > 18446744.0 || limitedHigh == 18446744.0 && limitedLow >= 73709551615.0 then
        maxValue

    else if limitedHigh == 0 then
        floor limitedLow

    else
        -- givenHigh * 1e12 + limitedLow
        mul (floor limitedHigh) (UInt64 ( 0, 0xE8D4, 0x00A51000 ))
            |> add (floor limitedLow)


{-| Convert 64-bit unsigned integer represented as three 24-bit unsigned integers to [`UInt64`](#UInt64).

This is the internal format of [`UInt64`](#UInt64)
and so [`fromInt24s`](#fromInt24s) is the fastest way to create [`UInt64`](#UInt64) value.

  - `high`: 16-bit unsigned integer for bits 48 - 63
  - `mid`: 24-bit unsigned integer for bits 24 - 47
  - `low`: 24-bit unsigned integer for bits 0 - 23

See [argument handling](#argument-handling).

    UInt64.fromInt24s 0x1122 0x334455 0x667788
        |> UInt64.toHexString
        --> "1122334455667788"

    UInt64.fromInt24s 1 2 3
        |> UInt64.toHexString
        --> "0001000002000003"

-}
fromInt24s : Int -> Int -> Int -> UInt64
fromInt24s high mid low =
    UInt64 ( Bitwise.and max16 high, Bitwise.and max24 mid, Bitwise.and max24 low )


{-| Convert 64-bit unsigned integer represented as two 32-bit unsigned integers to [`UInt64`](#UInt64).

  - `high`: 32-bit unsigned integer for bits 32 - 63
  - `low`: 32-bit unsigned integer for bits 0 - 31

See [argument handling](#argument-handling).

    UInt64.fromInt32s 0x11223344 0xAABBCCDD
        |> UInt64.toHexString
        --> "11223344AABBCCDD"

    UInt64.fromInt32s 1 2
        |> UInt64.toHexString
        --> "0000000100000002"

-}
fromInt32s : Int -> Int -> UInt64
fromInt32s high32 low32 =
    -- HHHHMMMMMMLLLLLL
    -- hhhhhhhhllllllll
    UInt64
        ( Bitwise.shiftRightZfBy 16 high32
        , Bitwise.and max24 <| Bitwise.or (Bitwise.shiftLeftBy 8 high32) (Bitwise.shiftRightZfBy 24 low32)
        , Bitwise.and max24 low32
        )


{-| Convert [`UInt64`](#UInt64) to list of 8 bytes in big-endian order.

    UInt64.fromInt 0xABCDEF
        |> UInt64.toBigEndianBytes
        --> [ 0, 0, 0, 0, 0, 0xAB, 0xCD, 0xEF ]

-}
toBigEndianBytes : UInt64 -> List Int
toBigEndianBytes (UInt64 ( high, mid, low )) =
    [ Bitwise.shiftRightZfBy 8 high
    , Bitwise.and 0xFF high
    , Bitwise.shiftRightZfBy 16 mid
    , Bitwise.and 0xFF <| Bitwise.shiftRightZfBy 8 mid
    , Bitwise.and 0xFF mid
    , Bitwise.shiftRightZfBy 16 low
    , Bitwise.and 0xFF <| Bitwise.shiftRightZfBy 8 low
    , Bitwise.and 0xFF low
    ]


{-| Convert [`UInt64`](#UInt64) to 64-bit unsigned integer represented as three 24-bit unsigned integers.

This is the internal format of [`UInt64`](#UInt64)
and so [`toInt24s`](#toInt24s) is the fastest way to extract value out of [`UInt64`](#UInt64).

  - `high`: 16-bit unsigned integer for bits 48 - 63
  - `mid`: 24-bit unsigned integer for bits 24 - 47
  - `low`: 24-bit unsigned integer for bits 0 - 23


## Example

    UInt64.fromInt32s 0x11223344 0xAABBCCDD
        |> UInt64.toInt24s
        --> ( 0x1122, 0x3344AA, 0xBBCCDD )

-}
toInt24s : UInt64 -> ( Int, Int, Int )
toInt24s (UInt64 tuple) =
    tuple


{-| Convert [`UInt64`](#UInt64) to 64-bit unsigned integer represented as two 32-bit unsigned integers.

  - `high`: 32-bit unsigned integer for bits 32 - 63
  - `low`: 32-bit unsigned integer for bits 0 - 31

See [large `Int`](#large-int-) note.

    UInt64.floor 1e15
        |> UInt64.toInt32s
        --> ( 0x00038D7E, 0xA4C68000 )

-}
toInt32s : UInt64 -> ( Int, Int )
toInt32s (UInt64 ( high, mid, low )) =
    -- HHHHMMMMMMLLLLLL
    -- hhhhhhhhllllllll
    ( Bitwise.shiftRightZfBy 0 <| Bitwise.or (Bitwise.shiftLeftBy 16 high) (Bitwise.shiftRightZfBy 8 mid)
    , Bitwise.shiftRightZfBy 0 <| Bitwise.or (Bitwise.shiftLeftBy 24 mid) low
    )



-- CONVERSION - STRING


{-| Convert `String` to [`UInt64`](#UInt64).

`String` can be

  - decimal `String` of digits `0123456789`
  - hexadecimal `String` with prefix `0x` and digits `0123456789ABCDEFabcdef`
  - octal `String` with prefix `0o` and digits `01234567`
  - binary `String` with prefix `0b` and digits `01`

Return `Nothing` if `String` isn't valid for any of the above formats,
or if the value would be above [`maxValue`](#maxValue).

See [`String` at argument handling](#-string-).

    UInt64.fromString "12345"
        |> Maybe.andThen UInt64.toInt31
        --> Just 12345

    UInt64.fromString "0x11223344AABBCCDD"
        |> Maybe.map UInt64.toInt32s
        --> Just ( 0x11223344, 0xAABBCCDD )

    UInt64.fromString "0o777"
        |> Maybe.map UInt64.toHexString
        --> Just "00000000000001FF"

    UInt64.fromString "0b1111000011110000"
        |> Maybe.map UInt64.toHexString
        --> Just "000000000000F0F0"

    -- `e` is not valid without `0x` prefix
    UInt64.fromString "1e10"
        --> Nothing

    -- value would be above `maxValue`
    UInt64.fromString "111222333444555666777"
        --> Nothing

-}
fromString : String -> Maybe UInt64
fromString str =
    let
        fromNonDecimalString charToDigit bitsPerDigit drop2 =
            case String.toList drop2 of
                [] ->
                    Nothing

                nonEmptyChars ->
                    riskyFromNonEmptyNonDecimalChars charToDigit bitsPerDigit nonEmptyChars
    in
    case String.uncons str of
        Just ( '0', drop1 ) ->
            case String.uncons drop1 of
                Just ( 'x', drop2 ) ->
                    fromNonDecimalString charToHexDigit 4 drop2

                Just ( 'o', drop2 ) ->
                    fromNonDecimalString charToOctalDigit 3 drop2

                Just ( 'b', drop2 ) ->
                    fromNonDecimalString charToBinaryDigit 1 drop2

                _ ->
                    fromDecimalString str

        _ ->
            fromDecimalString str


{-| Convert [`UInt64`](#UInt64) to uppercase hexadecimal `String` of 16 characters.

    UInt64.floor 1e15
        |> UInt64.toHexString
        --> "00038D7EA4C68000"

    UInt64.zero
        |> UInt64.toHexString
        --> "0000000000000000"

**Note:** See [Conversion - Digits](#conversion-digits) for more options
converting [`UInt64`](#UInt64) to`String`.

-}
toHexString : UInt64 -> String
toHexString (UInt64 ( high, mid, low )) =
    String.fromList
        [ riskyIntToUpperCaseDigit <| Bitwise.and 0x0F <| Bitwise.shiftRightZfBy 12 high
        , riskyIntToUpperCaseDigit <| Bitwise.and 0x0F <| Bitwise.shiftRightZfBy 8 high
        , riskyIntToUpperCaseDigit <| Bitwise.and 0x0F <| Bitwise.shiftRightZfBy 4 high
        , riskyIntToUpperCaseDigit <| Bitwise.and 0x0F high
        , riskyIntToUpperCaseDigit <| Bitwise.shiftRightZfBy 20 mid
        , riskyIntToUpperCaseDigit <| Bitwise.and 0x0F <| Bitwise.shiftRightZfBy 16 mid
        , riskyIntToUpperCaseDigit <| Bitwise.and 0x0F <| Bitwise.shiftRightZfBy 12 mid
        , riskyIntToUpperCaseDigit <| Bitwise.and 0x0F <| Bitwise.shiftRightZfBy 8 mid
        , riskyIntToUpperCaseDigit <| Bitwise.and 0x0F <| Bitwise.shiftRightZfBy 4 mid
        , riskyIntToUpperCaseDigit <| Bitwise.and 0x0F mid
        , riskyIntToUpperCaseDigit <| Bitwise.shiftRightZfBy 20 low
        , riskyIntToUpperCaseDigit <| Bitwise.and 0x0F <| Bitwise.shiftRightZfBy 16 low
        , riskyIntToUpperCaseDigit <| Bitwise.and 0x0F <| Bitwise.shiftRightZfBy 12 low
        , riskyIntToUpperCaseDigit <| Bitwise.and 0x0F <| Bitwise.shiftRightZfBy 8 low
        , riskyIntToUpperCaseDigit <| Bitwise.and 0x0F <| Bitwise.shiftRightZfBy 4 low
        , riskyIntToUpperCaseDigit <| Bitwise.and 0x0F low
        ]


{-| Convert [`UInt64`](#UInt64) to decimal `String`.

    UInt64.fromInt 0xFFFFFF
        |> UInt64.toString
        --> "16777215"

**Note:** See [Conversion - Digits](#conversion-digits) for more options
converting [`UInt64`](#UInt64) to`String`.

-}
toString : UInt64 -> String
toString x =
    -- `UInt64` is split to two parts
    -- > maxValue = 1844674407370|9551615
    -- > `divisor < 2^29` allows faster division
    -- > `lowDecimal < 2^24` fits within 24-bit `low` part
    -- > `highDecimal < 2^48` fits within 48-bit `mid/low` parts
    -- > `highDecimal < maxSafe` can be converted with `String.fromInt`
    let
        divisor =
            UInt64 ( 0, 0, 10000000 )

        ( highDecimal, lowDecimal ) =
            divMod x divisor

        highDecimalToDigits (UInt64 ( _, mid, low )) =
            String.fromInt <| low + limit24 * mid

        lowDecimalToDigits (UInt64 ( _, _, low )) =
            String.fromInt low
    in
    if isZero highDecimal then
        lowDecimalToDigits lowDecimal

    else
        highDecimalToDigits highDecimal
            ++ (String.padLeft 7 '0' <| lowDecimalToDigits lowDecimal)



-- CONVERSION - DIGITS


{-| Convert [`UInt64`](#UInt64) to [`Digits`](UInt64.Digits#Digits) of `Char`
using given [`Base`](UInt64.Digits#Base).

This is intended as first step in converting [`UInt64`](#UInt64) to `String`.

    import UInt64
    import UInt64.Digits as Digits

    UInt64.maxValue
        |> UInt64.toDigits Digits.octal
        |> Digits.toString
        --> "1777777777777777777777"

    UInt64.floor 1e15
        |> UInt64.toDigits Digits.hexLower
        |> Digits.padToMultipleOf 4 '0'
        |> Digits.groupToString 4 ' '
        --> "0003 8d7e a4c6 8000"

-}
toDigits : Base -> UInt64 -> Digits Char
toDigits base x =
    if isZero x then
        Digits ( 1, [ '0' ] )

    else
        case base of
            Decimal ->
                let
                    digitsStr =
                        toString x
                in
                Digits ( String.length digitsStr, String.toList digitsStr )

            Hex upperOrLower ->
                let
                    (Digits ( digitCount, digitsInt )) =
                        toIntDigits base x
                in
                if upperOrLower == Internal.Upper then
                    Digits ( digitCount, List.map riskyIntToUpperCaseDigit digitsInt )

                else
                    Digits ( digitCount, List.map riskyIntToLowerCaseDigit digitsInt )

            Octal ->
                let
                    (Digits ( digitCount, digitsInt )) =
                        toIntDigits base x
                in
                Digits ( digitCount, List.map riskyIntToUpperCaseDigit digitsInt )

            Binary ->
                let
                    (Digits ( digitCount, digitsInt )) =
                        toIntDigits base x
                in
                Digits ( digitCount, List.map riskyIntToUpperCaseDigit digitsInt )


{-| Convert [`UInt64`](#UInt64) to [`Digits`](UInt64.Digits#Digits) of `Int`
using given [`Base`](UInt64.Digits#Base).

This is like [`toDigits`](#toDigits) except that each digit will be `Int` instead of `Char`.

    import UInt64
    import UInt64.Digits as Digits

    UInt64.fromInt 0xABC
        |> UInt64.toIntDigits Digits.hex
        |> Digits.toList
        --> [ 10, 11, 12 ]

    -- digit sum of 1234 is `1+2+3+4 = 10`
    UInt64.fromInt 1234
        |> UInt64.toIntDigits Digits.decimal
        |> Digits.toList
        |> List.sum
        --> 10

-}
toIntDigits : Base -> UInt64 -> Digits Int
toIntDigits base x =
    let
        toNonDecimalIntDigits bitsPerDigit partToDigits (UInt64 ( high, mid, low )) =
            if high > 0 then
                let
                    ( highCount, highDigits ) =
                        riskyIntToNonDecimalIntDigits bitsPerDigit high 0 []
                in
                Digits
                    ( highCount + 48 // bitsPerDigit
                    , highDigits ++ partToDigits mid ++ partToDigits low
                    )

            else if mid > 0 then
                let
                    ( midCount, midDigits ) =
                        riskyIntToNonDecimalIntDigits bitsPerDigit mid 0 []
                in
                Digits ( midCount + 24 // bitsPerDigit, midDigits ++ partToDigits low )

            else
                Digits <| riskyIntToNonDecimalIntDigits bitsPerDigit low 0 []
    in
    if isZero x then
        Digits ( 1, [ 0 ] )

    else
        case base of
            Decimal ->
                toDecimalIntDigits x

            Hex _ ->
                let
                    partToDigits part =
                        [ Bitwise.shiftRightZfBy 20 part
                        , Bitwise.and 0x0F <| Bitwise.shiftRightZfBy 16 part
                        , Bitwise.and 0x0F <| Bitwise.shiftRightZfBy 12 part
                        , Bitwise.and 0x0F <| Bitwise.shiftRightZfBy 8 part
                        , Bitwise.and 0x0F <| Bitwise.shiftRightZfBy 4 part
                        , Bitwise.and 0x0F part
                        ]
                in
                toNonDecimalIntDigits 4 partToDigits x

            Octal ->
                let
                    partToDigits part =
                        [ Bitwise.shiftRightZfBy 21 part
                        , Bitwise.and 0x07 <| Bitwise.shiftRightZfBy 18 part
                        , Bitwise.and 0x07 <| Bitwise.shiftRightZfBy 15 part
                        , Bitwise.and 0x07 <| Bitwise.shiftRightZfBy 12 part
                        , Bitwise.and 0x07 <| Bitwise.shiftRightZfBy 9 part
                        , Bitwise.and 0x07 <| Bitwise.shiftRightZfBy 6 part
                        , Bitwise.and 0x07 <| Bitwise.shiftRightZfBy 3 part
                        , Bitwise.and 0x07 part
                        ]
                in
                toNonDecimalIntDigits 3 partToDigits x

            Binary ->
                let
                    partToDigits part =
                        [ Bitwise.shiftRightZfBy 23 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 22 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 21 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 20 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 19 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 18 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 17 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 16 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 15 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 14 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 13 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 12 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 11 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 10 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 9 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 8 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 7 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 6 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 5 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 4 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 3 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 2 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 1 part
                        , Bitwise.and 0x01 part
                        ]
                in
                toNonDecimalIntDigits 1 partToDigits x



-- MATH


{-| Addition with wrapping overflow.

    -- `123 + 456`
    UInt64.add (UInt64.fromInt 123) (UInt64.fromInt 456)
        |> UInt64.toString
        --> "579"

    -- `maxValue + 100`
    UInt64.add UInt64.maxValue (UInt64.fromInt 100)
        |> UInt64.toString
        --> "99"

-}
add : UInt64 -> UInt64 -> UInt64
add (UInt64 ( highA, midA, lowA )) (UInt64 ( highB, midB, lowB )) =
    let
        low =
            lowA + lowB

        mid =
            if low < limit24 then
                midA + midB

            else
                midA + midB + 1

        high =
            if mid < limit24 then
                highA + highB

            else
                highA + highB + 1
    in
    UInt64 ( Bitwise.and max16 high, Bitwise.and max24 mid, Bitwise.and max24 low )


{-| Decrement by one with wrapping overflow.

    -- `42 - 1`
    UInt64.decrement (UInt64.fromInt 42)
        |> UInt64.toString
        --> "41"

    -- `0 - 1`
    UInt64.decrement UInt64.zero
        |> UInt64.toHexString
        --> "FFFFFFFFFFFFFFFF"

-}
decrement : UInt64 -> UInt64
decrement (UInt64 ( high, mid, low )) =
    if low > 0 then
        UInt64 ( high, mid, low - 1 )

    else if mid > 0 then
        UInt64 ( high, mid - 1, max24 )

    else if high > 0 then
        UInt64 ( high - 1, max24, max24 )

    else
        maxValue


{-| Increment by one with wrapping overflow.

    -- `42 + 1`
    UInt64.increment (UInt64.fromInt 42)
        |> UInt64.toString
        --> "43"

    -- `maxValue + 1`
    UInt64.increment UInt64.maxValue
        |> UInt64.toString
        --> "0"

-}
increment : UInt64 -> UInt64
increment (UInt64 ( high, mid, low )) =
    if low < max24 then
        UInt64 ( high, mid, low + 1 )

    else if mid < max24 then
        UInt64 ( high, mid + 1, 0 )

    else if high < max16 then
        UInt64 ( high + 1, 0, 0 )

    else
        zero


{-| Multiplication with wrapping overflow.

    -- `1e9 * 1e9`
    UInt64.mul (UInt64.floor 1e9) (UInt64.floor 1e9)
        |> UInt64.toString
        --> "1000000000000000000"

    -- `(1e10 * 1e10) % 2^64`
    UInt64.mul (UInt64.floor 1e10) (UInt64.floor 1e10)
        |> UInt64.toString
        --> "7766279631452241920"

-}
mul : UInt64 -> UInt64 -> UInt64
mul (UInt64 ( highA, midA, lowA )) (UInt64 ( highB, midB, lowB )) =
    let
        lowFull =
            lowA * lowB

        lowCarry =
            Basics.floor <| Basics.toFloat lowFull / limit24

        low =
            lowFull - lowCarry * limit24

        midFull =
            lowCarry + lowA * midB + midA * lowB

        midCarry =
            Basics.floor <| Basics.toFloat midFull / limit24

        mid =
            midFull - midCarry * limit24

        high =
            Bitwise.and max16 (midCarry + lowA * highB + midA * midB + highA * lowB)
    in
    UInt64 ( high, mid, low )


{-| Power aka exponentiation. `0 ^ 0 = 1`

    -- `3 ^ 7`
    UInt64.pow (UInt64.fromInt 3) (UInt64.fromInt 7)
        |> UInt64.toString
        --> "2187"

    -- `10 ^ 19`
    UInt64.pow (UInt64.fromInt 10) (UInt64.fromInt 19)
        |> UInt64.toString
        --> "10000000000000000000"

    -- `(3 ^ 10000000000000000000) % 2^64`
    UInt64.pow (UInt64.fromInt 10) (UInt64.fromInt 19)
        |> UInt64.pow (UInt64.fromInt 3)
        |> UInt64.toString
        --> "12038004833498693633"

**Note:** Uses fast algorithms: Bases 0-2 are special-cased,
exponents 0-16 use [addition-chain exponentiation][AC]🢅 and
exponents over 16 use [exponentiation by squaring][ES]🢅.

[AC]: https://en.wikipedia.org/wiki/Addition-chain_exponentiation
[ES]: https://en.wikipedia.org/wiki/Exponentiation_by_squaring

-}
pow : UInt64 -> UInt64 -> UInt64
pow ((UInt64 ( baseHigh, baseMid, baseLow )) as base) ((UInt64 ( expHigh, expMid, expLow )) as exponent) =
    if baseLow <= 2 && baseMid == 0 && baseHigh == 0 then
        case baseLow of
            0 ->
                if isZero exponent then
                    -- Wikipedia says `0^0` is commonly either undefined or `1`.
                    -- Since `UInt64` doesn't support undefined, it's `1`.
                    -- > https://en.wikipedia.org/wiki/Zero_to_the_power_of_zero
                    one

                else
                    zero

            1 ->
                one

            _ ->
                -- baseLow == 2
                if expLow <= 63 && expMid == 0 && expHigh == 0 then
                    setBit expLow 1 zero

                else
                    zero

    else if expLow <= 16 && expMid == 0 && expHigh == 0 then
        -- I think that handling exponents 0-16 fast is good balance between speed and code complexity.
        -- If someone needs further exponents handled by addition-chain exponentiation,
        -- then such a function should probably be implemented separately in another package.
        -- > https://en.wikipedia.org/wiki/Addition-chain_exponentiation
        let
            cube x =
                mul x <| square x
        in
        case expLow of
            0 ->
                one

            1 ->
                base

            2 ->
                square base

            3 ->
                cube base

            4 ->
                square <| square base

            5 ->
                mul base <| square <| square base

            6 ->
                cube <| square base

            7 ->
                mul base <| cube <| square base

            8 ->
                square <| square <| square base

            9 ->
                cube <| cube base

            10 ->
                let
                    base2 =
                        square base
                in
                mul base2 <| square <| square base2

            11 ->
                let
                    base2 =
                        square base
                in
                mul base <| mul base2 <| square <| square base2

            12 ->
                cube <| square <| square base

            13 ->
                mul base <| cube <| square <| square base

            14 ->
                let
                    base2 =
                        square base
                in
                mul base2 <| cube <| square <| square base

            15 ->
                cube <| mul base <| square <| square base

            _ ->
                -- expLow == 16
                square <| square <| square <| square base

    else
        powHelper one base exponent


powHelper : UInt64 -> UInt64 -> UInt64 -> UInt64
powHelper multiplier base ((UInt64 ( expHigh, expMid, expLow )) as exponent) =
    -- tail recursive algorithm from https://en.wikipedia.org/wiki/Exponentiation_by_squaring
    --
    -- Algorithm
    -- - INVARIANT: value of `multiplier * base ^ exponent` doesn't change
    -- - begin with `1 * base ^ exponent`
    -- - half exponent at each step, increasing either `multiplier` or `base` to keep invariant
    -- - when exponent reaches 0 or 1, return `multiplier * base ^ exponent`
    --   - which is just `multiplier` (if exponent is 0) or `multiplier * base` (if exponent is 1)
    if expLow <= 1 && expMid == 0 && expHigh == 0 then
        if expLow == 0 then
            -- m * b ^ 0 == m
            multiplier

        else
            -- m * b ^ 1 == m * b
            mul multiplier base

    else if Bitwise.and 0x01 expLow == 0 then
        -- even exponent
        --   m * b ^ e == m * (b * b) ^ (e / 2)
        powHelper multiplier (square base) (shiftRightZfBy1 exponent)

    else
        -- odd exponent
        --   m * b ^ e == (m * b) * (b * b) ^ ((e - 1) / 2)
        powHelper (mul multiplier base) (square base) (shiftRightZfBy1 exponent)


{-| Squaring with wrapping overflow.

`square a` is same as `mul a a` or `pow a two` but faster.

    -- `1e9 * 1e9`
    UInt64.square (UInt64.floor 1e9)
        |> UInt64.toString
        --> "1000000000000000000"

-}
square : UInt64 -> UInt64
square (UInt64 ( high, mid, low )) =
    let
        lowDouble =
            Bitwise.shiftLeftBy 1 low

        lowFull =
            low * low

        lowCarry =
            Basics.floor <| Basics.toFloat lowFull / limit24

        lowX =
            lowFull - lowCarry * limit24

        midFull =
            lowCarry + lowDouble * mid

        midCarry =
            Basics.floor <| Basics.toFloat midFull / limit24

        midX =
            midFull - midCarry * limit24

        highX =
            Bitwise.and max16 (midCarry + mid * mid + lowDouble * high)
    in
    UInt64 ( highX, midX, lowX )


{-| Subtraction with wrapping overflow.

    -- `456 - 123`
    UInt64.sub (UInt64.fromInt 456) (UInt64.fromInt 123)
        |> UInt64.toString
        --> "333"

    -- `0 - 0xFF`
    UInt64.sub UInt64.zero (UInt64.fromInt 0xFF)
        |> UInt64.toHexString
        --> "FFFFFFFFFFFFFF01"

-}
sub : UInt64 -> UInt64 -> UInt64
sub (UInt64 ( highA, midA, lowA )) (UInt64 ( highB, midB, lowB )) =
    let
        low =
            lowA - lowB

        mid =
            if low >= 0 then
                midA - midB

            else
                midA - midB - 1

        high =
            if mid >= 0 then
                highA - highB

            else
                highA - highB - 1
    in
    UInt64 ( Bitwise.and max16 high, Bitwise.and max24 mid, Bitwise.and max24 low )



-- DIVISION


{-| Integer division.

  - If divisor is [`zero`](#zero), return [`zero`](#zero).


## Examples

    UInt64.div (UInt64.fromInt 1234) (UInt64.fromInt 100)
        |> UInt64.toFloat
        --> 12

    -- 0xFFFFFFFFFFFFFFFF / 1e10
    UInt64.div UInt64.maxValue (UInt64.floor 1e10)
        |> UInt64.toFloat
        --> 1844674407

-}
div : UInt64 -> UInt64 -> UInt64
div dividend ((UInt64 ( divisorHigh, divisorMid, divisorLow )) as divisor) =
    -- TODO: Initial analysis suggests that it might be possible to implement
    --       this without `divModFast`. Maybe even with a single faster algorithm.
    if divisorMid <= 0x1F && divisorHigh == 0 then
        -- divisor < 2^29
        if divisorLow == 0 && divisorMid == 0 then
            zero

        else
            -- TODO: ??? Copied from `divMod`. What limit applies here when modulo is not calculated. ???
            -- `divisor` can be at most 29 bits
            -- Limiting factor is that `highMidCarry * limit24 + dividendLow` <= `2 ^ 53 - 1`.
            let
                (UInt64 ( dividendHigh, dividendMid, dividendLow )) =
                    dividend

                dividendHighMid =
                    dividendHigh * limit24 + dividendMid

                divisorFloat =
                    toFloat divisor

                quotHighMid =
                    Basics.floor <| Basics.toFloat dividendHighMid / divisorFloat

                highMidCarry =
                    Basics.toFloat dividendHighMid - divisorFloat * Basics.toFloat quotHighMid

                highMidCarryWithLow =
                    highMidCarry * limit24 + Basics.toFloat dividendLow

                quotLow =
                    Basics.floor <| highMidCarryWithLow / divisorFloat

                quotHigh =
                    Basics.floor <| Basics.toFloat quotHighMid / limit24

                quotMid =
                    quotHighMid - quotHigh * limit24
            in
            UInt64 ( quotHigh, quotMid, quotLow )

    else if isSafe dividend then
        -- TODO: ??? Copied from `divMod`. What limit applies here when modulo is not calculated. ???
        -- dividend < 2^53 && divisor >= 2^29
        --
        -- !!! divisor can be outside safe range !!!
        -- --> quotInt  = 0
        -- --> algorithm works correctly and returns zero
        let
            dividendFloat =
                toFloat dividend

            divisorFloat =
                toFloat divisor

            quotInt =
                Basics.floor <| dividendFloat / divisorFloat

            quotHigh =
                Basics.floor <| Basics.toFloat quotInt / limit48

            quotMidLow =
                quotInt - limit48 * quotHigh

            quotMid =
                Basics.floor <| Basics.toFloat quotMidLow / limit24

            quotLow =
                quotMidLow - limit24 * quotMid
        in
        UInt64 ( quotHigh, quotMid, quotLow )

    else
        -- dividend >= 2^53 && divisor >= 2^29
        case divModFast dividend divisor of
            Ok ( div_, _ ) ->
                div_

            Err _ ->
                -- IMPOSSIBLE: I believe this case is impossible to reach
                -- But in case the impossible happens, use the slow&simple algorithm.
                Tuple.first <| divModSlow dividend divisor


{-| Integer division with modulo.

`divMod a b` is same as `( div a b, mod a b )` but faster.

  - If divisor is [`zero`](#zero), return `( zero, zero )`.


## Examples

    UInt64.divMod (UInt64.fromInt 1234) (UInt64.fromInt 100)
        |> Tuple.mapBoth UInt64.toFloat UInt64.toFloat
        --> ( 12, 34 )

    -- ( 0xFFFFFFFFFFFFFFFF / 1e10, 0xFFFFFFFFFFFFFFFF % 1e10 )
    UInt64.divMod UInt64.maxValue (UInt64.floor 1e10)
        |> Tuple.mapBoth UInt64.toFloat UInt64.toFloat
        --> ( 1844674407, 3709551615 )

-}
divMod : UInt64 -> UInt64 -> ( UInt64, UInt64 )
divMod dividend ((UInt64 ( divisorHigh, divisorMid, divisorLow )) as divisor) =
    if divisorMid <= 0x1F && divisorHigh == 0 then
        -- divisor < 2^29
        if divisorLow == 0 && divisorMid == 0 then
            ( zero, zero )

        else
            -- `divisor` can be at most 29 bits
            -- Limiting factor is that `highMidCarry * limit24 + dividendLow` <= `2 ^ 53 - 1`.
            let
                (UInt64 ( dividendHigh, dividendMid, dividendLow )) =
                    dividend

                dividendHighMid =
                    dividendHigh * limit24 + dividendMid

                divisorFloat =
                    toFloat divisor

                quotHighMid =
                    Basics.floor <| Basics.toFloat dividendHighMid / divisorFloat

                highMidCarry =
                    Basics.toFloat dividendHighMid - divisorFloat * Basics.toFloat quotHighMid

                highMidCarryWithLow =
                    highMidCarry * limit24 + Basics.toFloat dividendLow

                quotLow =
                    Basics.floor <| highMidCarryWithLow / divisorFloat

                quotHigh =
                    Basics.floor <| Basics.toFloat quotHighMid / limit24

                quotMid =
                    quotHighMid - quotHigh * limit24

                modMidLow =
                    highMidCarryWithLow - divisorFloat * Basics.toFloat quotLow

                modMid =
                    Basics.floor <| modMidLow / limit24

                modLow =
                    Basics.floor modMidLow - modMid * limit24
            in
            ( UInt64 ( quotHigh, quotMid, quotLow )
            , UInt64 ( 0, modMid, modLow )
            )

    else if isSafe dividend then
        -- dividend < 2^53 && divisor >= 2^29
        --
        -- !!! divisor can be outside safe range !!!
        -- --> quotInt  = 0
        -- --> modFloat = dividendFloat
        -- --> algorithm works correctly and returns ( zero, dividend )
        let
            dividendFloat =
                toFloat dividend

            divisorFloat =
                toFloat divisor

            quotInt =
                Basics.floor <| dividendFloat / divisorFloat

            quotHigh =
                Basics.floor <| Basics.toFloat quotInt / limit48

            quotMidLow =
                quotInt - limit48 * quotHigh

            quotMid =
                Basics.floor <| Basics.toFloat quotMidLow / limit24

            quotLow =
                quotMidLow - limit24 * quotMid

            modFloat =
                dividendFloat - divisorFloat * Basics.toFloat quotInt

            modHigh =
                Basics.floor <| modFloat / limit48

            modMidLow =
                modFloat - limit48 * Basics.toFloat modHigh

            modMid =
                Basics.floor <| modMidLow / limit24

            modLow =
                modMidLow - limit24 * Basics.toFloat modMid
        in
        ( UInt64 ( quotHigh, quotMid, quotLow )
        , UInt64 ( modHigh, modMid, Basics.floor modLow )
        )

    else
        -- dividend >= 2^53 && divisor >= 2^29
        case divModFast dividend divisor of
            Ok divMod_ ->
                divMod_

            Err _ ->
                -- IMPOSSIBLE: I believe this case is impossible to reach
                -- But in case the impossible happens, use the slow&simple algorithm.
                divModSlow dividend divisor


{-| Remainder after [`div`](#div).

  - If divisor is [`zero`](#zero), return [`zero`](#zero).


## Examples

    UInt64.mod (UInt64.fromInt 1234) (UInt64.fromInt 100)
        |> UInt64.toFloat
        --> 34

    -- 0xFFFFFFFFFFFFFFFF % 1e10
    UInt64.mod UInt64.maxValue (UInt64.floor 1e10)
        |> UInt64.toFloat
        --> 3709551615

-}
mod : UInt64 -> UInt64 -> UInt64
mod dividend ((UInt64 ( divisorHigh, divisorMid, divisorLow )) as divisor) =
    if divisorMid <= 0x1F && divisorHigh == 0 then
        -- divisor < 2^29
        if divisorLow == 0 && divisorMid == 0 then
            zero

        else
            -- `divisor` can be at most 29 bits
            -- Limiting factor is that `highMidCarry * limit24 + dividendLow` <= `2 ^ 53 - 1`.
            let
                (UInt64 ( dividendHigh, dividendMid, dividendLow )) =
                    dividend

                dividendHighMid =
                    dividendHigh * limit24 + dividendMid

                divisorFloat =
                    toFloat divisor

                quotHighMid =
                    Basics.floor <| Basics.toFloat dividendHighMid / divisorFloat

                highMidCarry =
                    Basics.toFloat dividendHighMid - divisorFloat * Basics.toFloat quotHighMid

                highMidCarryWithLow =
                    highMidCarry * limit24 + Basics.toFloat dividendLow

                quotLow =
                    Basics.floor <| highMidCarryWithLow / divisorFloat

                modMidLow =
                    highMidCarryWithLow - divisorFloat * Basics.toFloat quotLow

                modMid =
                    Basics.floor <| modMidLow / limit24

                modLow =
                    Basics.floor modMidLow - modMid * limit24
            in
            UInt64 ( 0, modMid, modLow )

    else if isSafe dividend then
        -- dividend < 2^53 && divisor >= 2^29
        --
        -- !!! divisor can be outside safe range !!!
        -- --> quotInt  = 0
        -- --> modFloat = dividendFloat
        -- --> algorithm works correctly and returns `dividend`
        let
            dividendFloat =
                toFloat dividend

            divisorFloat =
                toFloat divisor

            quotInt =
                Basics.floor <| dividendFloat / divisorFloat

            modFloat =
                dividendFloat - divisorFloat * Basics.toFloat quotInt

            modHigh =
                Basics.floor <| modFloat / limit48

            modMidLow =
                modFloat - limit48 * Basics.toFloat modHigh

            modMid =
                Basics.floor <| modMidLow / limit24

            modLow =
                modMidLow - limit24 * Basics.toFloat modMid
        in
        UInt64 ( modHigh, modMid, Basics.floor modLow )

    else
        -- dividend >= 2^53 && divisor >= 2^29
        case divModFast dividend divisor of
            Ok ( _, mod_ ) ->
                mod_

            Err _ ->
                -- IMPOSSIBLE: I believe this case is impossible to reach
                -- But in case the impossible happens, use the slow&simple algorithm.
                Tuple.second <| divModSlow dividend divisor



-- DIVISION - EXTRA


{-| Fast but complex algorithm for integer division with modulo.

  - If divisor is [`zero`](#zero), return `Ok ( zero, zero )`.
  - If impossible happens, return `Err error`.
      - This should never happen, but see [`divModSlow`](#divModSlow).

You should usually use [`divMod`](#divMod) instead because it will use
even faster algorithms when `dividend < 2^53` or `divisor < 2^29`,
and will fall back to [`divModFast`](#divModFast) otherwise.

So [`divModFast`](#divModFast) is faster than [`divMod`](#divMod) only when
`dividend >= 2^53 && divisor >= 2^29`.

-}
divModFast : UInt64 -> UInt64 -> Result String ( UInt64, UInt64 )
divModFast dividend divisor =
    -- !!! This is custom algorithm made up by myself from scratch !!!
    -- I'm surprised it actually seems to be working ... :D
    --
    -- Basic idea:
    --   STEP 1) calculate approximate division using floating point division
    --   STEP 2) analyze error of approximation and handle it
    --
    if isZero divisor then
        -- be consistent with other division algorithms
        Ok ( zero, zero )

    else
        let
            -- approximate divisor (exact if `isSafe divisor`)
            approxDivisor =
                toFloat divisor

            -- approximate division (exact if `isSafe dividend && isSafe divisor`)
            approxDiv =
                riskyFloatTo64 <| floorAnyPositiveFloat <| toFloat dividend / approxDivisor

            -- only 65 bits are actually needed, but implementing UInt72 is exactly as simple and fast as UInt65
            approxDivMultiplied72 : UInt72
            approxDivMultiplied72 =
                mul64_64_72 approxDiv divisor
        in
        -- compare `approxDiv` to `dividend / divisor`   <==>   compare `approxDiv * divisor` to `dividend`
        ------------------==============================
        case compare72_64 approxDivMultiplied72 dividend of
            --------------==============================
            EQ ->
                -- *** approxDivMultiplied72 == dividend ***
                -- approxDiv is exactly correct, which means modulo is zero
                Ok ( approxDiv, zero )

            LT ->
                -- *** approxDivMultiplied72 < dividend ***
                -- approxDiv is smaller than `dividend / divisor`. Then check the delta ...
                let
                    -- `dividend - approxDiv * divisor`, this could be the modulo ...
                    delta =
                        sub dividend (risky72to64 approxDivMultiplied72)
                in
                -- is `delta` small enough to be modulo ?
                -------------=============
                case compare delta divisor of
                    ---------=============
                    LT ->
                        -- *** delta < divisor ***
                        -- delta is small enough to be modulo, so we have the answer
                        Ok ( approxDiv, delta )

                    EQ ->
                        -- *** delta == divisor ***
                        -- modulo can't equal divisor, but this means `approxDiv` is just 1 too small and real modulo is zero
                        Ok ( increment approxDiv, zero )

                    GT ->
                        -- *** delta > divisor ***
                        -- delta is too large to be modulo, so `approxDiv` is far too small
                        let
                            -- add `delta / approxDivisor` to `approxDiv` to create new approximation
                            secondApproxDiv =
                                add
                                    approxDiv
                                    (riskyFloatTo64 <| floorAnyPositiveFloat <| toFloat delta / approxDivisor)

                            secondApproxDivMultiplied =
                                mul secondApproxDiv divisor
                        in
                        -- compare `secondApproxDiv` to `dividend / divisor`
                        -------------==================================
                        case compare secondApproxDivMultiplied dividend of
                            ---------==================================
                            EQ ->
                                -- *** secondApproxDivMultiplied == dividend ***
                                -- secondApproxDiv is exactly correct, which means modulo is zero
                                Ok ( secondApproxDiv, zero )

                            LT ->
                                -- *** secondApproxDivMultiplied < dividend ***
                                -- secondApproxDiv is smaller than `dividend / divisor`. Then check the delta ...
                                let
                                    -- `dividend - approxDiv * divisor`, this could be the modulo ...
                                    secondDelta =
                                        sub dividend secondApproxDivMultiplied
                                in
                                -- is `secondDelta` small enough to be modulo ?
                                -------------===================
                                case compare secondDelta divisor of
                                    ---------===================
                                    LT ->
                                        -- *** secondDelta < divisor ***
                                        -- secondDelta is small enough to be modulo, so we have the answer
                                        Ok ( secondApproxDiv, secondDelta )

                                    _ ->
                                        -- *** secondDelta >= divisor ***
                                        -- IMPOSSIBLE: I believe this case is impossible to reach
                                        Err "IMPOSSIBLE: secondDelta >= divisor"

                            GT ->
                                -- *** secondApproxDivMultiplied > dividend ***
                                -- IMPOSSIBLE: I believe this case is impossible to reach
                                Err "IMPOSSIBLE: secondApproxDivMultiplied > dividend"

            GT ->
                -- *** approxDivMultiplied72 > dividend ***
                -- approxDiv is larger than `dividend / divisor`
                -- This means that approxDiv will need fixing, no matter how large delta72 is. Then check the delta72 ...
                let
                    delta72 : UInt72
                    delta72 =
                        sub72_64_72 approxDivMultiplied72 dividend
                in
                -- is `delta72` small enough to be modulo ?
                ------------------===============
                case compare72_64 delta72 divisor of
                    --------------===============
                    LT ->
                        -- *** delta72 < divisor ***
                        -- delta72 is small enough to be modulo. This means `approxDiv` is just 1 too large,
                        --   so fix `approxDiv` and calculate correct modulo
                        Ok ( decrement approxDiv, sub divisor (risky72to64 delta72) )

                    EQ ->
                        -- *** delta72 == divisor ***
                        -- modulo can't equal divisor, but this means `approxDiv` is just 1 too large and real modulo is zero
                        -- NOTE: I've only seen this case being used with small divisors.
                        --       Largest divisor seen so far is 1923 with `UInt64.maxValue / 1923`.
                        Ok ( decrement approxDiv, zero )

                    GT ->
                        -- *** delta72 > divisor ***
                        -- delta72 is too large to be modulo, so `approxDiv` is far too large
                        -- NOTE: I've only seen this case being used with small divisors.
                        --       Largest divisor seen so far is 972 with `UInt64.maxValue / 972`.
                        -- ==> I'll limit this logic to `isSafe` values, so exact integer-Float math can be used for speed
                        if isSafe72 delta72 then
                            let
                                -- split `delta72` to div/mod pair
                                -- Since here `divisor < delta72 <= maxSafeAsFloat`, it follows that `approxDivisor` is exact
                                ( deltaDiv, deltaMod ) =
                                    divModFloat (toFloat72 delta72) approxDivisor
                            in
                            if deltaMod == 0 then
                                -- modulo is zero, so only `approxDiv` needs fixing
                                Ok ( sub approxDiv (riskyFloatTo64 deltaDiv), zero )

                            else
                                -- modulo is non-zero, so both `approxDiv` and modulo needs fixing
                                Ok ( sub approxDiv (riskyFloatTo64 <| deltaDiv + 1), riskyFloatTo64 <| approxDivisor - deltaMod )

                        else
                            -- IMPOSSIBLE: I believe this case is impossible to reach
                            Err "IMPOSSIBLE: approxDivMultiplied72 > dividend && not (isSafe72 delta72)"


{-| Simple but slow [long division][LD]🢅 algorithm for integer division with modulo.

  - If divisor is [`zero`](#zero), return `( zero, zero )`.

Intended use cases:

  - Benchmarking against other algorithms.
  - Used as fallback algorithm with [`divModFast`](#divModFast),
    in case it returns `Err`. Which should never happen.

[LD]: https://en.wikipedia.org/wiki/Division_algorithm#Long_division

-}
divModSlow : UInt64 -> UInt64 -> ( UInt64, UInt64 )
divModSlow dividend divisor =
    if isZero divisor then
        ( zero, zero )

    else
        List.foldl
            (\n ( div_, mod_ ) ->
                let
                    newMod =
                        setBit 0 (getBit n dividend) <| shiftLeftBy 1 mod_
                in
                if compare newMod divisor /= LT then
                    ( setBit n 1 div_, sub newMod divisor )

                else
                    ( div_, newMod )
            )
            ( zero, zero )
            -- I could pre-create this list, but this algorithm is meant to be
            -- the simple 100% correct one with no possibility of error.
            (List.reverse <| List.range 0 63)



-- BITWISE


{-| Bitwise AND.

    UInt64.and
        (UInt64.fromInt32s 0x11223344 0xAABBCCDD)
        (UInt64.fromInt32s 0x0000FFFF 0xFFFF0000)
        |> UInt64.toHexString
        --> "00003344AABB0000"

-}
and : UInt64 -> UInt64 -> UInt64
and (UInt64 ( highA, midA, lowA )) (UInt64 ( highB, midB, lowB )) =
    UInt64
        ( Bitwise.and highA highB
        , Bitwise.and midA midB
        , Bitwise.and lowA lowB
        )


{-| Bitwise complement, aka bitwise NOT, aka one's complement.

    UInt64.fromInt32s 0x11223344 0xAABBCCDD
        |> UInt64.complement
        |> UInt64.toHexString
        --> "EEDDCCBB55443322"

-}
complement : UInt64 -> UInt64
complement (UInt64 ( high, mid, low )) =
    UInt64
        ( Bitwise.xor max16 high
        , Bitwise.xor max24 mid
        , Bitwise.xor max24 low
        )


{-| Return a bit.

  - `bitNumber`: `0 <= x <= 63`, least significant bit is `0`

See [argument handling](#argument-handling).

    UInt64.one
        |> UInt64.getBit 0
        --> 1

-}
getBit : Int -> UInt64 -> Int
getBit givenBitNumber (UInt64 ( high, mid, low )) =
    let
        bitNumber =
            Bitwise.and 0x3F givenBitNumber
    in
    if bitNumber < 24 then
        Bitwise.and 1 <| Bitwise.shiftRightZfBy bitNumber low

    else if bitNumber < 48 then
        Bitwise.and 1 <| Bitwise.shiftRightZfBy (bitNumber - 24) mid

    else
        -- bitNumber < 64
        Bitwise.and 1 <| Bitwise.shiftRightZfBy (bitNumber - 48) high


{-| Bitwise OR.

    UInt64.or
        (UInt64.fromInt32s 0x11223344 0xAABBCCDD)
        (UInt64.fromInt32s 0x0000FFFF 0xFFFF0000)
        |> UInt64.toHexString
        --> "1122FFFFFFFFCCDD"

-}
or : UInt64 -> UInt64 -> UInt64
or (UInt64 ( highA, midA, lowA )) (UInt64 ( highB, midB, lowB )) =
    UInt64
        ( Bitwise.or highA highB
        , Bitwise.or midA midB
        , Bitwise.or lowA lowB
        )


{-| Bitwise rotate left.

  - `shift`: `0 <= x <= 63`

See [argument handling](#argument-handling).

    UInt64.fromInt32s 0x11223344 0xAABBCCDD
        |> UInt64.rotateLeftBy 20
        |> UInt64.toHexString
        --> "344AABBCCDD11223"

-}
rotateLeftBy : Int -> UInt64 -> UInt64
rotateLeftBy givenShift (UInt64 ( high, mid, low )) =
    let
        n =
            Bitwise.and 0x3F givenShift
    in
    --  0: HHHH MMMMMM LLLLLL
    --     ---- ------ ------
    --  8: HHMM MMMMLL LLLLHH
    -- 16: MMMM MMLLLL LLHHHH --
    -- 24: MMMM LLLLLL HHHHMM --
    -- 32: MMLL LLLLHH HHMMMM
    -- 40: LLLL LLHHHH MMMMMM --
    -- 48: LLLL HHHHMM MMMMLL --
    -- 56: LLHH HHMMMM MMLLLL
    if n < 16 then
        UInt64
            ( Bitwise.and max16 <| Bitwise.or (Bitwise.shiftLeftBy n high) (Bitwise.shiftRightZfBy (24 - n) mid)
            , Bitwise.and max24 <| Bitwise.or (Bitwise.shiftLeftBy n mid) (Bitwise.shiftRightZfBy (24 - n) low)
            , Bitwise.and max24 <| Bitwise.or (Bitwise.shiftLeftBy n low) (Bitwise.shiftRightZfBy (16 - n) high)
            )

    else if n < 24 then
        UInt64
            ( Bitwise.and max16 <| Bitwise.shiftRightZfBy (24 - n) mid
            , Bitwise.and max24 <| Bitwise.or (Bitwise.shiftLeftBy n mid) (Bitwise.shiftRightZfBy (24 - n) low)
            , Bitwise.and max24 <|
                Bitwise.shiftLeftBy n low
                    + Bitwise.shiftLeftBy (n - 16) high
                    + Bitwise.shiftRightZfBy (40 - n) mid
            )

    else if n < 40 then
        UInt64
            ( Bitwise.and max16 <| Bitwise.or (Bitwise.shiftLeftBy (n - 24) mid) (Bitwise.shiftRightZfBy (48 - n) low)
            , Bitwise.and max24 <| Bitwise.or (Bitwise.shiftLeftBy (n - 24) low) (Bitwise.shiftRightZfBy (40 - n) high)
            , Bitwise.and max24 <| Bitwise.or (Bitwise.shiftLeftBy (n - 16) high) (Bitwise.shiftRightZfBy (40 - n) mid)
            )

    else if n < 48 then
        UInt64
            ( Bitwise.and max16 <| Bitwise.shiftRightZfBy (48 - n) low
            , Bitwise.and max24 <|
                Bitwise.shiftLeftBy (n - 24) low
                    + Bitwise.shiftLeftBy (n - 40) high
                    + Bitwise.shiftRightZfBy (64 - n) mid
            , Bitwise.and max24 <| Bitwise.or (Bitwise.shiftLeftBy (n - 40) mid) (Bitwise.shiftRightZfBy (64 - n) low)
            )

    else
        -- n < 64
        UInt64
            ( Bitwise.and max16 <| Bitwise.or (Bitwise.shiftLeftBy (n - 48) low) (Bitwise.shiftRightZfBy (64 - n) high)
            , Bitwise.and max24 <| Bitwise.or (Bitwise.shiftLeftBy (n - 40) high) (Bitwise.shiftRightZfBy (64 - n) mid)
            , Bitwise.and max24 <| Bitwise.or (Bitwise.shiftLeftBy (n - 40) mid) (Bitwise.shiftRightZfBy (64 - n) low)
            )


{-| Bitwise rotate right.

  - `shift`: `0 <= x <= 63`

See [argument handling](#argument-handling).

    UInt64.fromInt32s 0x11223344 0xAABBCCDD
        |> UInt64.rotateRightBy 20
        |> UInt64.toHexString
        --> "BCCDD11223344AAB"

-}
rotateRightBy : Int -> UInt64 -> UInt64
rotateRightBy givenShift (UInt64 ( high, mid, low )) =
    let
        n =
            Bitwise.and 0x3F givenShift
    in
    --  0: HHHH MMMMMM LLLLLL
    --     ---- ------ ------
    --  8: LLHH HHMMMM MMLLLL
    -- 16: LLLL HHHHMM MMMMLL --
    -- 24: LLLL LLHHHH MMMMMM --
    -- 32: MMLL LLLLHH HHMMMM
    -- 40: MMMM LLLLLL HHHHMM --
    -- 48: MMMM MMLLLL LLHHHH --
    -- 56: HHMM MMMMLL LLLLHH
    if n < 16 then
        UInt64
            ( Bitwise.and max16 <| Bitwise.or (Bitwise.shiftRightZfBy n high) (Bitwise.shiftLeftBy (16 - n) low)
            , Bitwise.and max24 <| Bitwise.or (Bitwise.shiftRightZfBy n mid) (Bitwise.shiftLeftBy (24 - n) high)
            , Bitwise.and max24 <| Bitwise.or (Bitwise.shiftRightZfBy n low) (Bitwise.shiftLeftBy (24 - n) mid)
            )

    else if n < 24 then
        UInt64
            ( Bitwise.and max16 <| Bitwise.shiftRightZfBy (n - 16) low
            , Bitwise.and max24 <|
                Bitwise.shiftRightZfBy n mid
                    + Bitwise.shiftLeftBy (24 - n) high
                    + Bitwise.shiftLeftBy (40 - n) low
            , Bitwise.and max24 <| Bitwise.or (Bitwise.shiftRightZfBy n low) (Bitwise.shiftLeftBy (24 - n) mid)
            )

    else if n < 40 then
        UInt64
            ( Bitwise.and max16 <| Bitwise.or (Bitwise.shiftRightZfBy (n - 16) low) (Bitwise.shiftLeftBy (40 - n) mid)
            , Bitwise.and max24 <| Bitwise.or (Bitwise.shiftRightZfBy (n - 24) high) (Bitwise.shiftLeftBy (40 - n) low)
            , Bitwise.and max24 <| Bitwise.or (Bitwise.shiftRightZfBy (n - 24) mid) (Bitwise.shiftLeftBy (48 - n) high)
            )

    else if n < 48 then
        UInt64
            ( Bitwise.and max16 <| Bitwise.shiftRightZfBy (n - 40) mid
            , Bitwise.and max24 <| Bitwise.or (Bitwise.shiftRightZfBy (n - 40) low) (Bitwise.shiftLeftBy (64 - n) mid)
            , Bitwise.and max24 <|
                Bitwise.shiftRightZfBy (n - 24) mid
                    + Bitwise.shiftLeftBy (48 - n) high
                    + Bitwise.shiftLeftBy (64 - n) low
            )

    else
        -- n < 64
        UInt64
            ( Bitwise.and max16 <| Bitwise.or (Bitwise.shiftRightZfBy (n - 40) mid) (Bitwise.shiftLeftBy (64 - n) high)
            , Bitwise.and max24 <| Bitwise.or (Bitwise.shiftRightZfBy (n - 40) low) (Bitwise.shiftLeftBy (64 - n) mid)
            , Bitwise.and max24 <| Bitwise.or (Bitwise.shiftRightZfBy (n - 48) high) (Bitwise.shiftLeftBy (64 - n) low)
            )


{-| Set a bit to given value.

  - `bitNumber`: `0 <= x <= 63`, least significant bit is `0`
  - `bitValue`: new value, either `0` or `1`

See [argument handling](#argument-handling).

    UInt64.zero
        |> UInt64.setBit 30 1
        |> UInt64.toHexString
        --> "0000000040000000"

-}
setBit : Int -> Int -> UInt64 -> UInt64
setBit givenBitNumber givenBitValue (UInt64 ( high, mid, low )) =
    let
        bitNumber =
            Bitwise.and 0x3F givenBitNumber

        bitValue =
            Bitwise.and 0x01 givenBitValue
    in
    if bitNumber < 24 then
        if bitValue == 0 then
            UInt64 ( high, mid, Bitwise.and low <| Bitwise.complement <| Bitwise.shiftLeftBy bitNumber 1 )

        else
            UInt64 ( high, mid, Bitwise.or low <| Bitwise.shiftLeftBy bitNumber 1 )

    else if bitNumber < 48 then
        if bitValue == 0 then
            UInt64 ( high, Bitwise.and mid <| Bitwise.complement <| Bitwise.shiftLeftBy (bitNumber - 24) 1, low )

        else
            UInt64 ( high, Bitwise.or mid <| Bitwise.shiftLeftBy (bitNumber - 24) 1, low )

    else if bitValue == 0 then
        -- bitNumber < 64
        UInt64 ( Bitwise.and high <| Bitwise.complement <| Bitwise.shiftLeftBy (bitNumber - 48) 1, mid, low )

    else
        -- bitNumber < 64
        UInt64 ( Bitwise.or high <| Bitwise.shiftLeftBy (bitNumber - 48) 1, mid, low )


{-| Bitwise shift left, filling with zeroes from right.

  - `shift`: `0 <= x <= 63`

See [argument handling](#argument-handling).

    UInt64.fromInt32s 0x11223344 0xAABBCCDD
        |> UInt64.shiftLeftBy 20
        |> UInt64.toHexString
        --> "344AABBCCDD00000"

-}
shiftLeftBy : Int -> UInt64 -> UInt64
shiftLeftBy givenShift (UInt64 ( high, mid, low )) =
    let
        n =
            Bitwise.and 0x3F givenShift
    in
    --  0: HHHH MMMMMM LLLLLL
    -- 24: MMMM LLLLLL 000000
    -- 48: LLLL 000000 000000
    if n < 24 then
        UInt64
            ( Bitwise.and max16 <| Bitwise.or (Bitwise.shiftLeftBy n high) (Bitwise.shiftRightZfBy (24 - n) mid)
            , Bitwise.and max24 <| Bitwise.or (Bitwise.shiftLeftBy n mid) (Bitwise.shiftRightZfBy (24 - n) low)
            , Bitwise.and max24 <| Bitwise.shiftLeftBy n low
            )

    else if n < 48 then
        UInt64
            ( Bitwise.and max16 <| Bitwise.or (Bitwise.shiftLeftBy (n - 24) mid) (Bitwise.shiftRightZfBy (48 - n) low)
            , Bitwise.and max24 <| Bitwise.shiftLeftBy (n - 24) low
            , 0
            )

    else
        -- n < 64
        UInt64
            ( Bitwise.and max16 <| Bitwise.shiftLeftBy (n - 48) low
            , 0
            , 0
            )


{-| Bitwise shift right, filling with zeroes from left.

  - `shift`: `0 <= x <= 63`

See [argument handling](#argument-handling).

    UInt64.fromInt32s 0x11223344 0xAABBCCDD
        |> UInt64.shiftRightZfBy 20
        |> UInt64.toHexString
        --> "0000011223344AAB"

-}
shiftRightZfBy : Int -> UInt64 -> UInt64
shiftRightZfBy givenShift (UInt64 ( high, mid, low )) =
    let
        n =
            Bitwise.and 0x3F givenShift
    in
    --  0: HHHH MMMMMM LLLLLL
    -- 16: 0000 HHHHMM MMMMLL
    -- 24: 0000 00HHHH MMMMMM
    -- 40: 0000 000000 HHHHMM
    -- 48: 0000 000000 00HHHH
    if n < 24 then
        -- MAYBE TODO: ??? Should this be split at 16 ???
        UInt64
            ( Bitwise.shiftRightZfBy n high
            , Bitwise.and max24 <| Bitwise.or (Bitwise.shiftRightZfBy n mid) (Bitwise.shiftLeftBy (24 - n) high)
            , Bitwise.and max24 <| Bitwise.or (Bitwise.shiftRightZfBy n low) (Bitwise.shiftLeftBy (24 - n) mid)
            )

    else if n < 48 then
        -- MAYBE TODO: ??? Should this be split at 40 ???
        UInt64
            ( 0
            , Bitwise.shiftRightZfBy (n - 24) high
            , Bitwise.and max24 <| Bitwise.or (Bitwise.shiftRightZfBy (n - 24) mid) (Bitwise.shiftLeftBy (48 - n) high)
            )

    else
        -- n < 64
        UInt64
            ( 0
            , 0
            , Bitwise.shiftRightZfBy (n - 48) high
            )


{-| Bitwise shift right by one bit, filling with zero from left.

`shiftRightZfBy1 a` is same as `shiftRightZfBy 1 a` but faster.

    UInt64.fromInt32s 0xEECCAA88 0x66442200
        |> UInt64.shiftRightZfBy1
        |> UInt64.toHexString
        --> "7766554433221100"

-}
shiftRightZfBy1 : UInt64 -> UInt64
shiftRightZfBy1 (UInt64 ( high, mid, low )) =
    UInt64
        ( Bitwise.shiftRightZfBy 1 high
        , Bitwise.and max24 <| Bitwise.or (Bitwise.shiftRightZfBy 1 mid) (Bitwise.shiftLeftBy 23 high)
        , Bitwise.and max24 <| Bitwise.or (Bitwise.shiftRightZfBy 1 low) (Bitwise.shiftLeftBy 23 mid)
        )


{-| Bitwise XOR.

    UInt64.xor
        (UInt64.fromInt32s 0x11223344 0xAABBCCDD)
        (UInt64.fromInt32s 0x0000FFFF 0xFFFF0000)
        |> UInt64.toHexString
        --> "1122CCBB5544CCDD"

-}
xor : UInt64 -> UInt64 -> UInt64
xor (UInt64 ( highA, midA, lowA )) (UInt64 ( highB, midB, lowB )) =
    UInt64
        ( Bitwise.xor highA highB
        , Bitwise.xor midA midB
        , Bitwise.xor lowA lowB
        )



-- COMPARISON


{-| Compare two [`UInt64`](#UInt64):s.

    UInt64.compare UInt64.zero UInt64.one
        --> LT

-}
compare : UInt64 -> UInt64 -> Basics.Order
compare (UInt64 ( highA, midA, lowA )) (UInt64 ( highB, midB, lowB )) =
    case Basics.compare highA highB of
        EQ ->
            case Basics.compare midA midB of
                EQ ->
                    Basics.compare lowA lowB

                midNotEq ->
                    midNotEq

        highNotEq ->
            highNotEq



-- CHECK


{-| Return `True` if argument is even.
-}
isEven : UInt64 -> Bool
isEven (UInt64 ( _, _, low )) =
    Bitwise.and 0x01 low == 0


{-| Return `True` if argument is odd.
-}
isOdd : UInt64 -> Bool
isOdd (UInt64 ( _, _, low )) =
    Bitwise.and 0x01 low == 1


{-| Return `True` if argument is safe integer.

A safe integer is an integer that

  - can be represented exacly as `Float` and
  - no other integer is rounded to as `Float`.

Unsigned integers from `0` to [`maxSafe`](#maxSafe) are safe integers.


## Example

For example `2^53` is not a safe integer.
While it can be represented exactly as `Float`,
there exists another integer `2^53 + 1` which is rounded to `2^53`:

    -- 2^53 + 1 (9007199254740993) is rounded to
    -- 2^53     (9007199254740992) when converted to Float
    UInt64.fromDecimal12s 9007 199254740993
        |> UInt64.toFloat
        --> 9007199254740992

    UInt64.fromDecimal12s 9007 199254740993
        |> UInt64.isSafe
        --> False

This happens because `2^53 + 1` can't be represented exactly as `Float`,
so it is rounded to another integer.

-}
isSafe : UInt64 -> Bool
isSafe (UInt64 ( high, _, _ )) =
    high <= maxSafeHighPart


{-| Return `True` if argument is [`zero`](#zero).

This is same as `(==)`[`zero`](#zero) but much faster.

**Note:** See [Performance Optimization][PO]🢅 for discussion about speed of `==` in Elm 0.19.1.

[PO]: https://discourse.elm-lang.org/t/performance-optimization/5105

-}
isZero : UInt64 -> Bool
isZero (UInt64 ( high, mid, low )) =
    low == 0 && mid == 0 && high == 0



-- INTERNAL - TYPES


type alias UInt16 =
    Int


type alias UInt24 =
    Int


{-| Internal type that uses 24 bits also for high-part.

So far I've only needed up to 65 bits,
but implementing 65 or 72 is equally fast,
and 72 is actually a bit simpler.

-}
type UInt72
    = UInt72 ( UInt24, UInt24, UInt24 )



-- INTERNAL - CONSTANTS


limit24 : number
limit24 =
    0x01000000


limit48 : number
limit48 =
    0x0001000000000000


max16 : number
max16 =
    0xFFFF


max24 : number
max24 =
    0x00FFFFFF


{-| Minimum value known for certain to be an integer, and so doesn't need to use `Basics.floor`.

`2 ^ 52 = 4503599627370496`

-}
minCertainInteger : Float
minCertainInteger =
    4503599627370496.0


maxSafeInt : Int
maxSafeInt =
    9007199254740991


{-| Maximum value of `high` part of UInt64/UInt72 within safe integer range
-}
maxSafeHighPart : Int
maxSafeHighPart =
    0x1F



-- INTERNAL - FLOAT


{-| Copied from SafeInt.Unchecked.divMod
-}
divModFloat : Float -> Float -> ( Float, Float )
divModFloat a b =
    let
        div_ =
            Basics.toFloat <| Basics.floor <| a / b
    in
    ( div_, a - b * div_ )


floorAnyPositiveFloat : Float -> Float
floorAnyPositiveFloat x =
    if x >= minCertainInteger then
        x

    else
        Basics.toFloat <| Basics.floor x


{-| Float must be unsigned integer within safe range.
-}
riskyFloatTo64 : Float -> UInt64
riskyFloatTo64 x =
    let
        highMid =
            Basics.floor <| x / limit24

        low =
            Basics.floor x - highMid * limit24

        high =
            Basics.floor <| Basics.toFloat highMid / limit24

        mid =
            highMid - high * limit24
    in
    UInt64 ( high, mid, low )



-- INTERNAL - CHAR / DIGIT


{-| Convert chars to digits with given function.

  - Return `Nothing` if `charToDigit` returns `Nothing` for any char of `chars`.
  - Return also `Length.list digits` so it doesn't need to be calculated separately.
  - Initial call is with `digitCount = 0` and `digits = []`.

-}
charListToDigits : (Char -> Maybe Int) -> Int -> List Int -> List Char -> Maybe ( Int, List Int )
charListToDigits charToDigit digitCount digits chars =
    case chars of
        x :: xs ->
            case charToDigit x of
                Just digit ->
                    charListToDigits charToDigit (digitCount + 1) (digit :: digits) xs

                Nothing ->
                    Nothing

        [] ->
            Just ( digitCount, List.reverse digits )


charToBinaryDigit : Char -> Maybe Int
charToBinaryDigit char =
    case char of
        '0' ->
            Just 0

        '1' ->
            Just 1

        _ ->
            Nothing


charToHexDigit : Char -> Maybe Int
charToHexDigit char =
    case char of
        '0' ->
            Just 0

        '1' ->
            Just 1

        '2' ->
            Just 2

        '3' ->
            Just 3

        '4' ->
            Just 4

        '5' ->
            Just 5

        '6' ->
            Just 6

        '7' ->
            Just 7

        '8' ->
            Just 8

        '9' ->
            Just 9

        'A' ->
            Just 10

        'B' ->
            Just 11

        'C' ->
            Just 12

        'D' ->
            Just 13

        'E' ->
            Just 14

        'F' ->
            Just 15

        'a' ->
            Just 10

        'b' ->
            Just 11

        'c' ->
            Just 12

        'd' ->
            Just 13

        'e' ->
            Just 14

        'f' ->
            Just 15

        _ ->
            Nothing


charToOctalDigit : Char -> Maybe Int
charToOctalDigit char =
    case char of
        '0' ->
            Just 0

        '1' ->
            Just 1

        '2' ->
            Just 2

        '3' ->
            Just 3

        '4' ->
            Just 4

        '5' ->
            Just 5

        '6' ->
            Just 6

        '7' ->
            Just 7

        _ ->
            Nothing


fromDecimalString : String -> Maybe UInt64
fromDecimalString str =
    let
        length =
            String.length str
    in
    if String.startsWith "-" str || String.startsWith "+" str then
        Nothing

    else if length <= 15 then
        case String.toInt str of
            Just int ->
                Just <| fromInt int

            Nothing ->
                Nothing

    else if length <= 20 then
        let
            lowStr =
                String.right 10 str
        in
        if String.startsWith "-" lowStr || String.startsWith "+" lowStr then
            Nothing

        else
            case ( String.toInt <| String.dropRight 10 str, String.toInt lowStr ) of
                ( Just highDecimal, Just lowDecimal ) ->
                    -- maxValue = 1844674407|3709551615
                    if highDecimal > 1844674407 || (highDecimal == 1844674407 && lowDecimal > 3709551615) then
                        Nothing

                    else
                        -- highDecimal * 1e10 + lowDecimal
                        mul (fromInt highDecimal) (UInt64 ( 0, 0x0254, 0x000BE400 ))
                            |> add (fromInt lowDecimal)
                            |> Just

                _ ->
                    Nothing

    else
        -- length > 20
        let
            prefix =
                String.dropRight 20 str
        in
        if String.toInt prefix /= Just 0 then
            Nothing

        else
            fromDecimalString <| String.right 20 str


{-| Convert list of digits to `UInt64`.

  - `bitsPerDigit` must be a factor of `24`.
  - `digitCount` must equal `List.length digits`.
  - Each digit must be `0 <= digit <= 2 ^ bitsPerDigit - 1`.
  - Return `Just zero` for empty list.
  - Return `Nothing` on overflow.

-}
riskyFromNonDecimalDigits : Int -> Int -> List Int -> Maybe UInt64
riskyFromNonDecimalDigits bitsPerDigit digitCount digits =
    let
        bitCount =
            digitCount * bitsPerDigit
    in
    if bitCount <= 66 then
        -- 22 octal digits is 66 bits
        riskyFromNonDecimalDigitsHelper bitsPerDigit bitCount 0 0 0 digits

    else
        Nothing


{-| Convert hex/octal/binary digits to UInt64.

  - `bitsPerDigit` must be factor of `24`.
  - `bitCount` must equal `bitsPerDigit * List.length digits` and be within `0 <= bitCount <= 66`.
  - Each digit must be within `0 <= digit <= 2 ^ bitsPerDigit - 1`.
  - Initial call is with `high = mid = low = 0`.
  - Return `Nothing` on overflow.

p.s. This would also handle base-4 and base-64.

-}
riskyFromNonDecimalDigitsHelper : Int -> Int -> Int -> Int -> Int -> List Int -> Maybe UInt64
riskyFromNonDecimalDigitsHelper bitsPerDigit bitCount high mid low digits =
    case digits of
        x :: xs ->
            if bitCount <= 24 then
                riskyFromNonDecimalDigitsHelper
                    bitsPerDigit
                    (bitCount - bitsPerDigit)
                    high
                    mid
                    (Bitwise.or x <| Bitwise.shiftLeftBy bitsPerDigit low)
                    xs

            else if bitCount <= 48 then
                riskyFromNonDecimalDigitsHelper
                    bitsPerDigit
                    (bitCount - bitsPerDigit)
                    high
                    (Bitwise.or x <| Bitwise.shiftLeftBy bitsPerDigit mid)
                    low
                    xs

            else
                riskyFromNonDecimalDigitsHelper
                    bitsPerDigit
                    (bitCount - bitsPerDigit)
                    (Bitwise.or x <| Bitwise.shiftLeftBy bitsPerDigit high)
                    mid
                    low
                    xs

        [] ->
            if high > max16 then
                Nothing

            else
                Just <| UInt64 ( high, mid, low )


{-| Convert hex/octal/binary chars to digits and then to `UInt64`, ignoring leading zeroes.

  - `bitsPerDigit` must be a factor of `24`.
  - Return `Just zero` if `chars` is empty or has only zeroes.
  - Return `Nothing` if `charListToDigits` or `riskyFromNonDecimalDigits` returns `Nothing`.

-}
riskyFromNonEmptyNonDecimalChars : (Char -> Maybe Int) -> Int -> List Char -> Maybe UInt64
riskyFromNonEmptyNonDecimalChars charToDigit bitsPerDigit chars =
    case chars of
        '0' :: xs ->
            riskyFromNonEmptyNonDecimalChars charToDigit bitsPerDigit xs

        noLeadingZeroes ->
            case charListToDigits charToDigit 0 [] noLeadingZeroes of
                Just ( digitCount, digits ) ->
                    riskyFromNonDecimalDigits bitsPerDigit digitCount digits

                Nothing ->
                    Nothing


{-| Convert `Int` to decimal digits of `0 <= digit <= 9`.

  - Given `Int` must be `0 <= x < 2^31`.
  - Return `(0, [])` if `x == 0`.
  - Return also `Length.list digits` so it doesn't need to be calculated separately.
  - Initial call is with `digitCount = 0` and `digits = []`.

-}
riskyIntToDecimalIntDigits : Int -> Int -> List Int -> ( Int, List Int )
riskyIntToDecimalIntDigits x digitCount digits =
    if x == 0 then
        ( digitCount, digits )

    else
        riskyIntToDecimalIntDigits (x // 10) (digitCount + 1) (modBy 10 x :: digits)


{-| Convert `Int` to lowercase `Char` digit. Return `x` for values over 15.

Works for binary/octal/decimal/hex as long as argument is valid.

-}
riskyIntToLowerCaseDigit : Int -> Char
riskyIntToLowerCaseDigit x =
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
            'a'

        11 ->
            'b'

        12 ->
            'c'

        13 ->
            'd'

        14 ->
            'e'

        15 ->
            'f'

        _ ->
            'x'


{-| Convert `Int` to hex/octal/binary digits of `0 <= digit <= 2 ^ bitsPerDigit - 1`.

  - `bitsPerDigit` must be a factor of `24`.
  - Given `Int` must be `0 <= x < 2^31`.
  - Return `(0, [])` if `x == 0`.
  - Return also `Length.list digits` so it doesn't need to be calculated separately.
  - Initial call is with `digitCount = 0` and `digits = []`.

-}
riskyIntToNonDecimalIntDigits : Int -> Int -> Int -> List Int -> ( Int, List Int )
riskyIntToNonDecimalIntDigits bitsPerDigit x digitCount digits =
    let
        bitMask =
            Bitwise.shiftLeftBy bitsPerDigit 1 - 1
    in
    if x == 0 then
        ( digitCount, digits )

    else
        riskyIntToNonDecimalIntDigits
            bitsPerDigit
            (Bitwise.shiftRightZfBy bitsPerDigit x)
            (digitCount + 1)
            (Bitwise.and bitMask x :: digits)


{-| Convert `Int` to uppercase `Char` digit. Return `X` for values over 15.

Works for binary/octal/decimal/hex as long as argument is valid.

-}
riskyIntToUpperCaseDigit : Int -> Char
riskyIntToUpperCaseDigit x =
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
            'X'


toDecimalIntDigits : UInt64 -> Digits Int
toDecimalIntDigits x =
    -- `UInt64` is split to three 7-decimal-digit parts
    -- > maxValue = 184467|4407370|9551615
    -- > `divisor < 2^29` allows faster division
    -- > `each part < 2^31` allows `Int` math
    -- > `lowDecimal < 2^24` fits within 24-bit `low` part
    -- > `highMidDecimal < 2^48` fits within 48-bit `mid/low` parts
    let
        divisorInt =
            10000000

        divisor =
            UInt64 ( 0, 0, divisorInt )

        ( highMidDecimal, UInt64 ( _, _, lowDecimal ) ) =
            divMod x divisor
    in
    if isZero highMidDecimal then
        Digits <| riskyIntToDecimalIntDigits lowDecimal 0 []

    else
        let
            (UInt64 ( _, highMidDecimal_mid, highMidDecimal_low )) =
                highMidDecimal

            highMidDecimalInt =
                highMidDecimal_low + limit24 * highMidDecimal_mid

            highDecimal =
                Basics.floor <| Basics.toFloat highMidDecimalInt / divisorInt

            midDecimal =
                highMidDecimalInt - divisorInt * highDecimal
        in
        if highDecimal == 0 then
            let
                ( lowCount, lowDigits ) =
                    riskyIntToDecimalIntDigits lowDecimal 0 []

                ( midCount, midDigits ) =
                    riskyIntToDecimalIntDigits midDecimal 0 []
            in
            Digits ( midCount + 7, midDigits ++ List.repeat (7 - lowCount) 0 ++ lowDigits )

        else
            let
                ( lowCount, lowDigits ) =
                    riskyIntToDecimalIntDigits lowDecimal 0 []

                ( midCount, midDigits ) =
                    riskyIntToDecimalIntDigits midDecimal 0 []

                ( highCount, highDigits ) =
                    riskyIntToDecimalIntDigits highDecimal 0 []
            in
            Digits
                ( highCount + 14
                , highDigits
                    ++ List.repeat (7 - midCount) 0
                    ++ midDigits
                    ++ List.repeat (7 - lowCount) 0
                    ++ lowDigits
                )



-- INTERNAL - UInt72


compare72_64 : UInt72 -> UInt64 -> Basics.Order
compare72_64 (UInt72 ( highA, midA, lowA )) (UInt64 ( highB, midB, lowB )) =
    case Basics.compare highA highB of
        EQ ->
            case Basics.compare midA midB of
                EQ ->
                    Basics.compare lowA lowB

                midNotEq ->
                    midNotEq

        highNotEq ->
            highNotEq


isSafe72 : UInt72 -> Bool
isSafe72 (UInt72 ( high, _, _ )) =
    high <= maxSafeHighPart


mul64_64_72 : UInt64 -> UInt64 -> UInt72
mul64_64_72 (UInt64 ( highA, midA, lowA )) (UInt64 ( highB, midB, lowB )) =
    let
        lowFull =
            lowA * lowB

        lowCarry =
            Basics.floor <| Basics.toFloat lowFull / limit24

        low =
            lowFull - lowCarry * limit24

        midFull =
            lowCarry + lowA * midB + midA * lowB

        midCarry =
            Basics.floor <| Basics.toFloat midFull / limit24

        mid =
            midFull - midCarry * limit24

        high =
            Bitwise.and max24 (midCarry + lowA * highB + midA * midB + highA * lowB)
    in
    UInt72 ( high, mid, low )


{-| Use only when UInt72 is known to be < 2^64
-}
risky72to64 : UInt72 -> UInt64
risky72to64 (UInt72 tuple) =
    UInt64 tuple


sub72_64_72 : UInt72 -> UInt64 -> UInt72
sub72_64_72 (UInt72 ( highA, midA, lowA )) (UInt64 ( highB, midB, lowB )) =
    let
        low =
            lowA - lowB

        mid =
            if low >= 0 then
                midA - midB

            else
                midA - midB - 1

        high =
            if mid >= 0 then
                highA - highB

            else
                highA - highB - 1
    in
    UInt72 ( Bitwise.and max24 high, Bitwise.and max24 mid, Bitwise.and max24 low )


toFloat72 : UInt72 -> Float
toFloat72 (UInt72 ( high, mid, low )) =
    (Basics.toFloat high * limit24 + Basics.toFloat mid) * limit24 + Basics.toFloat low

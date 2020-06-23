module Benchmarks exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import SafeInt.Unchecked as Unchecked
import UInt64 exposing (UInt64)


main : BenchmarkProgram
main =
    program <|
        describe "All"
            [ -- ### MATH
              -- benchmark_math
              -- benchmark_pow_tiny
              -- benchmark_pow_small
              benchmark_pow_large

            -- ### BITWISE
            -- benchmark_bitwise
            -- ### DIVMOD
            -- benchmark_divMod
            -- ### DIVMOD - DIFFERENT ALGORITHMS
            -- benchmark_divMod_53
            -- benchmark_divMod_smallDivisor
            -- benchmark_divMod_largeDivisor
            ]



-- BENCHMARK - MATH


benchmark_math =
    describe "math"
        [ benchmark "add 64-bit 53-bit" <|
            \_ -> UInt64.add uint64Prime64 uint64Prime53
        , benchmark "sub 64-bit 53-bit" <|
            \_ -> UInt64.sub uint64Prime64 uint64Prime53
        , benchmark "mul 64-bit 53-bit" <|
            \_ -> UInt64.mul uint64Prime64 uint64Prime53
        ]


benchmark_pow_tiny =
    describe "pow"
        [ benchmark "64-bit ^ 0" <|
            \_ -> UInt64.pow uint64Prime64 UInt64.zero
        , benchmark "64-bit ^ 1" <|
            \_ -> UInt64.pow uint64Prime64 UInt64.one
        , benchmark "64-bit ^ 2" <|
            \_ -> UInt64.pow uint64Prime64 UInt64.two
        , benchmark "64-bit ^ 3" <|
            \_ -> UInt64.pow uint64Prime64 (UInt64.fromInt 3)
        ]


benchmark_pow_small =
    describe "pow"
        [ benchmark "64-bit ^ 4" <|
            \_ -> UInt64.pow uint64Prime64 (UInt64.fromInt 4)
        , benchmark "64-bit ^ 8" <|
            \_ -> UInt64.pow uint64Prime64 (UInt64.fromInt 8)
        , benchmark "64-bit ^ 16" <|
            \_ -> UInt64.pow uint64Prime64 (UInt64.fromInt 16)
        , benchmark "64-bit ^ 32" <|
            \_ -> UInt64.pow uint64Prime64 (UInt64.fromInt 32)
        ]


benchmark_pow_large =
    describe "pow"
        [ benchmark "64-bit ^ 8-bit" <|
            \_ -> UInt64.pow uint64Prime64 uint64Prime8
        , benchmark "64-bit ^ 16-bit" <|
            \_ -> UInt64.pow uint64Prime64 uint64Prime16
        , benchmark "64-bit ^ 32-bit" <|
            \_ -> UInt64.pow uint64Prime64 uint64Prime32
        , benchmark "64-bit ^ 64-bit" <|
            \_ -> UInt64.pow uint64Prime64 uint64Prime64
        ]



-- BENCHMARK - BITWISE


benchmark_bitwise =
    describe "bitwise"
        [ benchmark "shiftLeftBy 1" <|
            \_ -> UInt64.shiftLeftBy 1 uint64Prime64
        , benchmark "shiftRightZfBy 1" <|
            \_ -> UInt64.shiftRightZfBy 1 uint64Prime64
        , benchmark "rotateLeftBy 1" <|
            \_ -> UInt64.rotateLeftBy 1 uint64Prime64
        , benchmark "rotateRightBy 1" <|
            \_ -> UInt64.rotateRightBy 1 uint64Prime64
        ]



-- BENCHMARK - DIVMOD


benchmark_divMod =
    describe "UInt64.divMod with different sizes"
        [ benchmark "<53 bits (53-bit / 40-bit)" <|
            \_ -> UInt64.divMod uint64Prime53 uint64Prime40
        , benchmark "small divisor (64-bit / 29-bit)" <|
            \_ -> UInt64.divMod uint64Prime64 uint64Prime29
        , benchmark "large divisor (64-bit / 40-bit)" <|
            \_ -> UInt64.divMod uint64Prime64 uint64Prime40
        ]



-- BENCHMARK - DIVMOD - DIFFERENT ALGORITHMS


benchmark_divMod_53 =
    describe "divMod <53 bits (53-bit / 40-bit)"
        [ benchmark "SafeInt.Unchecked.divMod" <|
            \_ -> Unchecked.divMod floatPrime53 floatPrime40
        , benchmark "UInt64.divMod" <|
            \_ -> UInt64.divMod uint64Prime53 uint64Prime40
        , benchmark "UInt64.divModFast" <|
            \_ -> UInt64.divModFast uint64Prime53 uint64Prime40
        , benchmark "UInt64.divModSlow" <|
            \_ -> UInt64.divModSlow uint64Prime53 uint64Prime40
        ]


benchmark_divMod_smallDivisor =
    describe "divMod small divisor (64-bit / 29-bit)"
        [ benchmark "UInt64.divMod" <|
            \_ -> UInt64.divMod uint64Prime64 uint64Prime29
        , benchmark "UInt64.divModFast" <|
            \_ -> UInt64.divModFast uint64Prime64 uint64Prime29
        , benchmark "UInt64.divModSlow" <|
            \_ -> UInt64.divModSlow uint64Prime64 uint64Prime29
        ]


benchmark_divMod_largeDivisor =
    describe "divMod large divisor (64-bit / 40-bit)"
        [ benchmark "UInt64.divMod" <|
            \_ -> UInt64.divMod uint64Prime64 uint64Prime40
        , benchmark "UInt64.divModFast" <|
            \_ -> UInt64.divModFast uint64Prime64 uint64Prime40
        , benchmark "UInt64.divModSlow" <|
            \_ -> UInt64.divModSlow uint64Prime64 uint64Prime40
        ]



-- SOME PRIMES
-- Primes are from <https://primes.utm.edu/lists/2small/0bit.html>


{-| 8-bit prime `2^8 - 5 = 251 = 0xFB`
-}
uint64Prime8 : UInt64
uint64Prime8 =
    UInt64.fromInt24s 0 0 0xFB


{-| 16-bit prime `2^16 - 15 = 65521 = 0xFFF1`
-}
uint64Prime16 : UInt64
uint64Prime16 =
    UInt64.fromInt24s 0 0 0xFFF1


{-| 29-bit prime `2^29 - 3 = 536870909 = 0x1FFFFFFD`
-}
floatPrime29 : Float
floatPrime29 =
    536870909.0


uint64Prime29 : UInt64
uint64Prime29 =
    UInt64.fromInt24s 0 0x1F 0x00FFFFFD


{-| 32-bit prime `2^32 - 5 = 4294967291 = 0xFFFFFFFB`
-}
uint64Prime32 : UInt64
uint64Prime32 =
    UInt64.fromInt24s 0 0xFF 0x00FFFFFB


{-| 40-bit prime `2^40 - 87 = 1099511627689 = 0xFFFFFFFFA9`
-}
floatPrime40 : Float
floatPrime40 =
    1099511627689.0


uint64Prime40 : UInt64
uint64Prime40 =
    UInt64.fromInt24s 0 0xFFFF 0x00FFFFA9


{-| 53-bit prime `2^53 - 111 = 9007199254740881 = 0x1FFFFFFFFFFF91`
-}
floatPrime53 : Float
floatPrime53 =
    9007199254740881.0


uint64Prime53 : UInt64
uint64Prime53 =
    UInt64.fromInt24s 0x1F 0x00FFFFFF 0x00FFFF91


{-| 64-bit prime `2^64 - 59 = 18446744073709551557 = 0xFFFFFFFFFFFFFFC5`
-}
uint64Prime64 : UInt64
uint64Prime64 =
    UInt64.fromInt24s 0xFFFF 0x00FFFFFF 0x00FFFFC5

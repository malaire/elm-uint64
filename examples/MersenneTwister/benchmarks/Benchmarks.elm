module Benchmarks exposing (main)

import Array
import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import MersenneTwister
import UInt64


main : BenchmarkProgram
main =
    program <|
        describe "Mersenne Twister"
            [ benchmark "init" <|
                \_ -> MersenneTwister.init singleSeed
            , benchmark "initByArray" <|
                \_ -> MersenneTwister.initByArray arraySeed
            , benchmark "uint64 (with twist)" <|
                \_ ->
                    MersenneTwister.uint64 initializedMersenneTwister
            , benchmark "uint64 (without twist)" <|
                \_ ->
                    MersenneTwister.uint64 initializedAndTwistedMersenneTwister
            ]


singleSeed =
    UInt64.fromInt 19650218


arraySeed =
    Array.fromList
        [ UInt64.fromInt24s 0 0 0x00012345
        , UInt64.fromInt24s 0 0 0x00023456
        , UInt64.fromInt24s 0 0 0x00034567
        , UInt64.fromInt24s 0 0 0x00045678
        ]


initializedMersenneTwister =
    MersenneTwister.initByArray arraySeed


initializedAndTwistedMersenneTwister =
    MersenneTwister.initByArray arraySeed
        |> MersenneTwister.uint64
        |> Tuple.second

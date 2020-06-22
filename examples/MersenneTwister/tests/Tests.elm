module Tests exposing (all)

import Array
import Expect exposing (Expectation)
import MersenneTwister
import Test exposing (..)
import TestData
import UInt64


all =
    describe "Mersenne Twister"
        [ test "first 10 generated uint64:s are correct" <|
            \_ ->
                List.foldl
                    (\_ ( mt, accum ) ->
                        let
                            ( x, newMt ) =
                                MersenneTwister.uint64 mt
                        in
                        ( newMt, x :: accum )
                    )
                    ( MersenneTwister.initByArray seed, [] )
                    (List.range 1 10)
                    |> Tuple.second
                    |> List.reverse
                    |> List.map UInt64.toHexString
                    |> Expect.equal (List.take 10 TestData.output1000)
        , test "first 1000 generated uint64:s are correct" <|
            \_ ->
                List.foldl
                    (\_ ( mt, accum ) ->
                        let
                            ( x, newMt ) =
                                MersenneTwister.uint64 mt
                        in
                        ( newMt, x :: accum )
                    )
                    ( MersenneTwister.initByArray seed, [] )
                    (List.range 1 1000)
                    |> Tuple.second
                    |> List.reverse
                    |> List.map UInt64.toHexString
                    |> Expect.equal TestData.output1000
        ]


seed =
    Array.fromList
        [ UInt64.fromInt24s 0 0 0x00012345
        , UInt64.fromInt24s 0 0 0x00023456
        , UInt64.fromInt24s 0 0 0x00034567
        , UInt64.fromInt24s 0 0 0x00045678
        ]

module MersenneTwister exposing
    ( MersenneTwister
    , init
    , initByArray
    , uint64
    )

import Array exposing (Array)
import UInt64 exposing (UInt64)


type MersenneTwister
    = MersenneTwister
        { mt : Array UInt64
        , index : Int
        }



-- SETUP


nn =
    312


mm =
    156


matrixA =
    UInt64.fromInt24s 0xB502 0x006F5AA9 0x006619E9


upperMask =
    UInt64.fromInt24s 0xFFFF 0x00FFFF80 0x00


lowerMask =
    UInt64.fromInt24s 0 0x7F 0x00FFFFFF


temperingMaskA =
    UInt64.fromInt24s 0x5555 0x00555555 0x00555555


temperingMaskB =
    UInt64.fromInt24s 0x71D6 0x007FFFED 0x00A60000


temperingMaskC =
    UInt64.fromInt24s 0xFFF7 0x00EEE000 0


initMultiplier =
    -- 6364136223846793005
    UInt64.fromInt24s 0x5851 0x00F42D4C 0x00957F2D


initByArrayMultiplierA =
    -- 3935559000370003845
    UInt64.fromInt24s 0x369D 0x00EA0F31 0x00A53F85


initByArrayMultiplierB =
    -- 2862933555777941757
    UInt64.fromInt24s 0x27BB 0x002EE687 0x00B0B0FD



-- INTERNAL HELPERS


get : Int -> Array UInt64 -> UInt64
get index array =
    -- if algorithm is implemented correctly, this is never `Nothing`
    Array.get index array
        |> Maybe.withDefault UInt64.zero



-- MAIN FUNCTIONS


init : UInt64 -> MersenneTwister
init seed =
    MersenneTwister
        { index = nn
        , mt =
            List.foldl
                (\index ( head, tail ) ->
                    let
                        next =
                            UInt64.shiftRightZfBy 62 head
                                |> UInt64.xor head
                                |> UInt64.mul initMultiplier
                                |> UInt64.add (UInt64.fromInt index)
                    in
                    ( next, head :: tail )
                )
                ( seed, [] )
                (List.range 1 (nn - 1))
                |> (\( head, tail ) -> Array.fromList <| List.reverse <| head :: tail)
        }


initByArray : Array UInt64 -> MersenneTwister
initByArray key =
    let
        ( initialIndex, MersenneTwister { mt } ) =
            initByArrayFirstPart key

        ( _, newOuterMt ) =
            List.foldl
                (\_ ( index, innerMt ) ->
                    let
                        newAtIndex =
                            UInt64.sub
                                (UInt64.shiftRightZfBy 62 (get (index - 1) innerMt)
                                    |> UInt64.xor (get (index - 1) innerMt)
                                    |> UInt64.mul initByArrayMultiplierB
                                    |> UInt64.xor (get index innerMt)
                                )
                                (UInt64.fromInt index)

                        newInnerMt =
                            Array.set index newAtIndex innerMt
                    in
                    if index + 1 == nn then
                        ( 1, Array.set 0 (get (nn - 1) newInnerMt) newInnerMt )

                    else
                        ( index + 1, newInnerMt )
                )
                ( initialIndex, mt )
                (List.range 2 nn)
    in
    MersenneTwister
        { index = nn
        , mt = Array.set 0 (UInt64.setBit 63 1 UInt64.zero) newOuterMt
        }


initByArrayFirstPart : Array UInt64 -> ( Int, MersenneTwister )
initByArrayFirstPart key =
    let
        (MersenneTwister { mt }) =
            init <| UInt64.fromInt 19650218

        keyLength =
            Array.length key

        ( lastIndex, _, newOuterMt ) =
            List.foldl
                (\_ ( index, keyIndex, innerMt ) ->
                    let
                        newAtIndex =
                            UInt64.shiftRightZfBy 62 (get (index - 1) innerMt)
                                |> UInt64.xor (get (index - 1) innerMt)
                                |> UInt64.mul initByArrayMultiplierA
                                |> UInt64.xor (get index innerMt)
                                |> UInt64.add (get keyIndex key)
                                |> UInt64.add (UInt64.fromInt keyIndex)

                        newInnerMt =
                            Array.set index newAtIndex innerMt

                        newKeyIndex =
                            Basics.modBy keyLength (keyIndex + 1)
                    in
                    if index + 1 == nn then
                        ( 1, newKeyIndex, Array.set 0 (get (nn - 1) newInnerMt) newInnerMt )

                    else
                        ( index + 1, newKeyIndex, newInnerMt )
                )
                ( 1, 0, mt )
                (List.range 1 <| Basics.max nn keyLength)
    in
    ( lastIndex, MersenneTwister { index = nn, mt = newOuterMt } )


uint64 : MersenneTwister -> ( UInt64, MersenneTwister )
uint64 ((MersenneTwister { mt, index }) as mersenneTwister) =
    if index >= nn then
        uint64 <| twist mersenneTwister

    else
        let
            x0 =
                get index mt

            x1 =
                UInt64.xor x0 <| UInt64.and temperingMaskA <| UInt64.shiftRightZfBy 29 x0

            x2 =
                UInt64.xor x1 <| UInt64.and temperingMaskB <| UInt64.shiftLeftBy 17 x1

            x3 =
                UInt64.xor x2 <| UInt64.and temperingMaskC <| UInt64.shiftLeftBy 37 x2

            x4 =
                UInt64.xor x3 <| UInt64.shiftRightZfBy 43 x3
        in
        ( x4, MersenneTwister { mt = mt, index = index + 1 } )


twist : MersenneTwister -> MersenneTwister
twist (MersenneTwister { mt }) =
    let
        newMt =
            List.foldl
                (\index innerMt ->
                    let
                        x =
                            UInt64.and (get index innerMt) upperMask
                                |> UInt64.add (UInt64.and (get (Basics.modBy nn (index + 1)) innerMt) lowerMask)

                        xA =
                            if UInt64.getBit 0 x == 1 then
                                UInt64.xor (UInt64.shiftRightZfBy 1 x) matrixA

                            else
                                UInt64.shiftRightZfBy 1 x

                        newAtIndex =
                            UInt64.xor (get (Basics.modBy nn (index + mm)) innerMt) xA
                    in
                    Array.set index newAtIndex innerMt
                )
                mt
                (List.range 0 (nn - 1))
    in
    MersenneTwister { mt = newMt, index = 0 }

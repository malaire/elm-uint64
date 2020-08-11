module Calculator exposing (main)

import Bitwise
import Browser
import Browser.Dom as Dom
import Css as C
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Parser exposing ((|.), (|=), Parser)
import Pratt
import Task
import UInt64 exposing (UInt64)
import UInt64.Digits as Digits



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view >> H.toUnstyled
        }



-- MODEL


type alias Model =
    { expression : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { expression = "" }
    , Task.attempt (\_ -> FocusFailed) <| Dom.focus "expression"
    )



-- UPDATE


type Msg
    = FocusFailed
    | UserEnteredExpression String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FocusFailed ->
            ( model, Cmd.none )

        UserEnteredExpression expression ->
            ( { model | expression = expression }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    H.div [ A.css [ C.textAlign C.center, C.fontFamily C.monospace ] ]
        [ H.h2 [] [ H.text "UInt64 Calculator" ]
        , H.input
            [ A.id "expression"
            , A.type_ "text"
            , A.size 60
            , A.value model.expression
            , E.onInput UserEnteredExpression
            , A.css [ C.display C.block, C.margin C.auto ]
            ]
            []
        , H.br [] []
        , viewResult model.expression
        , H.br [] []
        , H.br [] []
        , H.text "Base Prefixes: 0x 0o 0b"
        , H.br [] []
        , H.text "Constants: max maxsafe maxfloat"
        , H.br [] []
        , H.br [] []
        , H.text "Operators"
        , H.table [ A.css [ C.margin C.auto ] ]
            [ H.tr [] [ H.td [] [ H.text "" ], H.td [] [ H.text "( )" ] ]
            , H.tr [] [ H.td [] [ H.text "8" ], H.td [] [ H.text "~" ] ]
            , H.tr [] [ H.td [] [ H.text "7" ], H.td [] [ H.text "^" ] ]
            , H.tr [] [ H.td [] [ H.text "6" ], H.td [] [ H.text "* / %" ] ]
            , H.tr [] [ H.td [] [ H.text "5" ], H.td [] [ H.text "+ -" ] ]
            , H.tr [] [ H.td [] [ H.text "4" ], H.td [] [ H.text "<< >> rol ror" ] ]
            , H.tr [] [ H.td [] [ H.text "3" ], H.td [] [ H.text "and" ] ]
            , H.tr [] [ H.td [] [ H.text "2" ], H.td [] [ H.text "xor" ] ]
            , H.tr [] [ H.td [] [ H.text "1" ], H.td [] [ H.text "or" ] ]
            ]
        , H.br [] []
        , H.br [] []
        , H.a [ A.href "https://github.com/malaire/elm-uint64/tree/master/examples/Calculator" ]
            [ H.text "README @ GitHub" ]
        ]


viewResult : String -> Html Msg
viewResult expression =
    let
        value =
            case Parser.run expressionStringParser expression of
                Ok value_ ->
                    Just value_

                Err _ ->
                    Nothing

        decimalStr =
            value
                |> Maybe.map (UInt64.toDigits Digits.decimal)
                |> Maybe.withDefault Digits.empty
                |> Digits.pad 20 '.'
                |> Digits.groupToString 3 ' '

        hexStr =
            value
                |> Maybe.map (UInt64.toDigits Digits.hex)
                |> Maybe.withDefault Digits.empty
                |> Digits.pad 16 '.'
                |> Digits.groupToString 4 ' '

        octalStr =
            value
                |> Maybe.map (UInt64.toDigits Digits.octal)
                |> Maybe.withDefault Digits.empty
                |> Digits.pad 22 '.'
                |> Digits.groupToString 4 ' '

        binaryStr =
            value
                |> Maybe.map (UInt64.toDigits Digits.binary)
                |> Maybe.withDefault Digits.empty
                |> Digits.pad 64 '.'
                |> Digits.groupToString 4 ' '
    in
    H.div [ A.css [ C.textAlign C.center, C.fontSize C.larger ] ]
        [ H.br [] []
        , H.text decimalStr
        , H.br [] []
        , H.text hexStr
        , H.br [] []
        , H.text octalStr
        , H.br [] []
        , H.text binaryStr
        ]



-- MATH


limitToInt6 : UInt64 -> Int
limitToInt6 x =
    UInt64.toInt32s x
        |> Tuple.second
        |> Bitwise.and 0x3F



-- PARSER HELPERS


isPossibleLiteralChar : Char -> Bool
isPossibleLiteralChar char =
    (char >= '0' && char <= '9')
        || (char >= 'A' && char <= 'F')
        || (char >= 'a' && char <= 'f')
        || (char == 'o')
        || (char == 'x')



-- PARSERS


literalParser : Parser UInt64
literalParser =
    Parser.succeed ()
        |. Parser.chompIf (\char -> char >= '0' && char <= '9')
        |. Parser.chompWhile isPossibleLiteralChar
        |> Parser.getChompedString
        |> Parser.andThen
            (\str ->
                case UInt64.fromString str of
                    Just value ->
                        Parser.succeed value

                    Nothing ->
                        Parser.problem "Invalid literal"
            )


expressionParser : Parser UInt64
expressionParser =
    Pratt.expression
        { oneOf =
            [ Pratt.constant (Parser.keyword "max") UInt64.maxValue
            , Pratt.constant (Parser.keyword "maxsafe") UInt64.maxSafe
            , Pratt.constant (Parser.keyword "maxfloat") UInt64.maxFloat
            , Pratt.literal literalParser
            , Pratt.prefix 8 (Parser.symbol "~") UInt64.complement
            , parenthesizedExpressionParser
            ]
        , andThenOneOf =
            [ Pratt.infixLeft 1 (Parser.keyword "or") UInt64.or
            , Pratt.infixLeft 2 (Parser.keyword "xor") UInt64.xor
            , Pratt.infixLeft 3 (Parser.keyword "and") UInt64.and
            , Pratt.infixLeft 4 (Parser.symbol "<<") (\a b -> UInt64.shiftLeftBy (limitToInt6 b) a)
            , Pratt.infixLeft 4 (Parser.symbol ">>") (\a b -> UInt64.shiftRightZfBy (limitToInt6 b) a)
            , Pratt.infixLeft 4 (Parser.keyword "rol") (\a b -> UInt64.rotateLeftBy (limitToInt6 b) a)
            , Pratt.infixLeft 4 (Parser.keyword "ror") (\a b -> UInt64.rotateRightBy (limitToInt6 b) a)
            , Pratt.infixLeft 5 (Parser.symbol "+") UInt64.add
            , Pratt.infixLeft 5 (Parser.symbol "-") UInt64.sub
            , Pratt.infixLeft 6 (Parser.symbol "*") UInt64.mul
            , Pratt.infixLeft 6 (Parser.symbol "/") UInt64.div
            , Pratt.infixLeft 6 (Parser.symbol "%") UInt64.mod
            , Pratt.infixRight 7 (Parser.symbol "^") UInt64.pow
            ]
        , spaces = Parser.spaces
        }


parenthesizedExpressionParser : Pratt.Config UInt64 -> Parser UInt64
parenthesizedExpressionParser config =
    Parser.succeed Basics.identity
        |. Parser.symbol "("
        |= Pratt.subExpression 0 config
        |. Parser.symbol ")"


expressionStringParser : Parser UInt64
expressionStringParser =
    Parser.succeed Basics.identity
        |= expressionParser
        |. Parser.end

module LongDiv exposing (main)

import Browser
import Debug
import Html exposing (Html, button, div, hr, input, text)
import Html.Attributes exposing (class, name, type_, value)
import Html.Events exposing (onClick, onInput)
import Question
import Random


type Msg
    = Quotient String
    | Remainder String


type alias Division =
    { divisor : Int, dividend : Int, quotient : Maybe Int, remainder : Maybe Int }


generator : Random.Generator Division
generator =
    Random.map3
        (\x y z ->
            { divisor = x, dividend = x * y + z, quotient = Nothing, remainder = Nothing }
        )
        (Random.int 3 17)
        (Random.int 7 250)
        (Random.int 0 17)


solve : Division -> Division
solve d =
    { d | quotient = Just <| d.dividend // d.divisor, remainder = Just <| modBy d.divisor d.dividend }


view : Division -> Question.Convert Msg -> Question.Qhtml Msg
view d f =
    { question = text "question"
    , answer = text "answer"
    }


update : Division -> Msg -> Division
update d m =
    case m of
        Quotient val ->
            { d | quotient = String.toInt val }

        Remainder val ->
            { d | remainder = String.toInt val }


main : Program () (Question.Model Division) (Question.Msg Msg)
main =
    Question.element
        { generator = generator
        , solve = solve
        , view = view
        , update = update
        }

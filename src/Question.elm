module Question exposing (..)

import Html exposing (Html, div)
import Random


type Status
    = Todo
    | Incorrect
    | Correct

type alias QuestionHtml m =
    { question: Html m
    , answer: Html m
    }

type alias Row q =
    {question : q, status: Status, id: Int}

type RowMsg m
    = QMsg Int m
    | NewSeed String
    | Cheat

type Class comparable_question msg
    = Class
        { generator : Random.Generator comparable_question
        , solve : comparable_question -> comparable_question
        , view : comparable_question -> (msg -> RowMsg msg) -> QuestionHtml (RowMsg msg)
        , update : comparable_question -> msg -> comparable_question
        }



check : Class comparable_q m -> comparable_q -> Status
check (Class c) q =
    if q == c.solve q then Correct else Incorrect

updateRow : Class comparable_q m -> RowMsg m -> Row comparable_q -> Row comparable_q
updateRow (Class c as x) rm r =
    case rm of
        (QMsg i m) ->
            if i == r.id then
                let
                    q = c.update r.question m
                in
                {r | question = q, status = check x q}
            else
                r
        _ -> r

viewRow : Class comparable_q m -> Row comparable_q -> Html (RowMsg m)
viewRow (Class c) r =
    let
        h : QuestionHtml (RowMsg m)
        h = c.view r.question (QMsg r.id)
    in
    div [] [h.question, h.answer]

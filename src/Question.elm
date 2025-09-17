module Question exposing (Convert, Model, Msg, Qhtml, element, unicode)

import Browser
import Html exposing (Html, button, div, h3, hr, input, text)
import Html.Attributes exposing (class, name, type_)
import Html.Events exposing (onClick, onInput)
import Random



-- Types ----------------------------------------------------------------------


type Status
    = Todo
    | Incorrect
    | Correct


type alias Qhtml m =
    { question : Html (Msg m)
    , answer : Html (Msg m)
    }


type alias Row q =
    { question : q, status : Status, id : Int }


type Msg m
    = QMsg Int m
    | NewSeed String
    | Cheat


type alias Convert m =
    m -> Msg m


type alias Class q msg =
    { generator : Random.Generator q
    , solve : q -> q
    , view : q -> Convert msg -> Qhtml msg
    , update : q -> msg -> q
    }


type alias Model q =
    { seed : Int
    , rows : List (Row q)
    , title : String
    }



-- Utility functions ----------------------------------------------------------


unicode n =
    " " ++ String.fromChar (Char.fromCode n) ++ " "


tick =
    unicode 0x2705


cross =
    unicode 0x274C


qmark =
    unicode 0x2753



-- Function to create Browser element -----------------------------------------


element : Class q m -> Program String (Model q) (Msg m)
element class =
    Browser.element
        { init = init class
        , view = view class
        , update = update class
        , subscriptions = \_ -> Sub.none
        }



-- init -----------------------------------------------------------------------


init : Class q m -> String -> ( Model q, Cmd (Msg m) )
init class title =
    ( modelFromSeed class title (Just 0), Cmd.none )


modelFromSeed : Class q m -> String -> Maybe Int -> Model q
modelFromSeed c title s =
    let
        seed =
            Maybe.withDefault 0 s

        gen =
            Random.list 10 c.generator

        qs =
            Random.step gen (Random.initialSeed seed) |> Tuple.first
    in
    { seed = seed
    , rows = List.indexedMap (\i q -> { question = q, status = Todo, id = i }) qs
    , title = title
    }



-- view -----------------------------------------------------------------------


view : Class q m -> Model q -> Html (Msg m)
view c m =
    --enterSeed
    h3 [] [ text m.title ]
        :: List.map (viewRow c) m.rows
        ++ [ div [] [ cheat, enterSeed ] ]
        |> List.intersperse (hr [] [])
        |> div []


viewRow : Class q m -> Row q -> Html (Msg m)
viewRow c r =
    let
        h : Qhtml m
        h =
            c.view r.question (QMsg r.id)

        s =
            case r.status of
                Todo ->
                    qmark

                Incorrect ->
                    cross

                Correct ->
                    tick
    in
    div [ class "question" ] [ h.question, h.answer, text s ]


cheat =
    button [ onClick Cheat ] [ text "CHEAT!" ]


enterSeed =
    div [] [ text "seed = ", input [ type_ "number", name "seed", onInput NewSeed ] [] ]



-- update ---------------------------------------------------------------------


update : Class q m -> Msg m -> Model q -> ( Model q, Cmd (Msg m) )
update c rm model =
    case rm of
        QMsg i m ->
            ( { model | rows = List.map (updateRow c i m) model.rows }, Cmd.none )

        NewSeed s ->
            ( modelFromSeed c model.title <| String.toInt s, Cmd.none )

        Cheat ->
            ( { model | rows = List.map (showAnswer c) model.rows }, Cmd.none )


updateRow : Class q m -> Int -> m -> Row q -> Row q
updateRow c id msg row =
    if id == row.id then
        let
            question =
                c.update row.question msg
        in
        { row
            | question = question
            , status = status c question
        }

    else
        row


showAnswer c row =
    let
        question =
            c.solve row.question
    in
    { row | question = question, status = status c question }


status c question =
    if question == c.solve question then
        Correct

    else
        Incorrect

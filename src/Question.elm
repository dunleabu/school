module Question exposing (..)

import Browser
import Html exposing (Html, div)
import Random


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


type alias Class zzz_question msg
    =
        { generator : Random.Generator zzz_question
        , solve : zzz_question -> zzz_question
        , view : zzz_question -> Convert msg -> Qhtml msg
        , update : zzz_question -> msg -> zzz_question
        }


type alias Model zzz_question =
    { seed : Int
    , rows : List (Row zzz_question)
    }



{-
   check : Class zzz_q m -> zzz_q -> Status
   check (Class c) q =
       if q == c.solve q then
           Correct

       else
           Incorrect
-}


updateRow : Class zzz_q m -> Int -> m -> Row zzz_q -> Row zzz_q
updateRow c id msg row =
    if id == row.id then
        let
            q =
                c.update row.question msg
        in
        { row
            | question = q
            , status =
                if q == c.solve q then
                    Correct

                else
                    Incorrect
        }

    else
        row


update : Class zzz_q m -> Msg m -> Model zzz_q -> ( Model zzz_q, Cmd (Msg m) )
update c rm model =
    case rm of
        QMsg i m ->
            ( { model | rows = List.map (updateRow c i m) model.rows }, Cmd.none )

        _ ->
            ( model, Cmd.none )


viewRow : Class zzz_q m -> Row zzz_q -> Html (Msg m)
viewRow c r =
    let
        h : Qhtml m
        h =
            c.view r.question (QMsg r.id)
    in
    div [] [ h.question, h.answer ]


modelFromSeed : Class q m -> Maybe Int -> Model q
modelFromSeed c s =
    let
        seed =
            Maybe.withDefault 0 s

        gen =
            Random.list 10 c.generator

        qs =
            Random.step gen (Random.initialSeed seed) |> Tuple.first
    in
    { seed = seed, rows = List.indexedMap (\i q -> { question = q, status = Todo, id = i }) qs }


view : Class q m -> Model q -> Html (Msg m)
view c m =
    div []
        (List.map (viewRow c) m.rows)


makeElement : Class q m -> Program () (Model q) (Msg m)
makeElement class =
    Browser.element
        { init = \_ -> ( modelFromSeed class (Just 0), Cmd.none )
        , view = view class
        , update = update class
        , subscriptions = \_ -> Sub.none
        }

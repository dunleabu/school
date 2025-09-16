module LongDiv exposing (main)

import Browser
import Debug
import Html exposing (Html, button, div, input, text, hr)
import Html.Attributes exposing (name, type_, value, class)
import Html.Events exposing (onClick, onInput)
import Random


type alias Model =
    { rows : List Row, seed : Int }


type QMsg
    = Quotient Int String
    | Remainder Int String


type Msg
    = NewSeed String
    | UpdateQuestion QMsg
    | Cheat


type Status
    = Todo
    | Incorrect
    | Correct


type Question
    = Division { divisor : Int, dividend : Int, quotient : Maybe Int, remainder : Maybe Int }


type alias Row =
    { question : Question, status : Status, index : Int }


checkQuestion : Question -> Status
checkQuestion (Division d) =
    case ( d.quotient, d.remainder ) of
        ( Just q, Just r ) ->
            let
                a =
                    r + q * d.divisor

                _ =
                    Debug.log "r = " r

                _ =
                    Debug.log "q = " q

                _ =
                    Debug.log "a = " a
            in
            if r + q * d.divisor == d.dividend then
                Correct

            else
                Incorrect

        _ ->
            Todo


solve : Question -> Question
solve (Division d) =
    Division { d | quotient = Just <| d.dividend // d.divisor, remainder = Just <| modBy d.divisor d.dividend }


showAnswers row =
    let
        q = solve row.question
    in
    { row | question = q, status = checkQuestion q }


updateQuotient : Maybe Int -> (Question -> Question)
updateQuotient value =
    \(Division d) -> Division { d | quotient = value }


updateRemainder : Maybe Int -> (Question -> Question)
updateRemainder value =
    \(Division d) -> Division { d | remainder = value }


makeRows : List Question -> List Row
makeRows qs =
    List.indexedMap
        (\i q -> Row q Todo i)
        qs


unicode n =
    " " ++ String.fromChar (Char.fromCode n) ++ " "


divideSymbol =
    unicode 0xF7


tick =
    unicode 0x2705


cross =
    unicode 0x274C


qmark =
    unicode 0x2753


rowHtml : Row -> Html Msg
rowHtml x =
    questionHtml x.question x.index ++ statusHtml x.status |> div [class "question"]


statusHtml : Status -> List (Html Msg)
statusHtml s =
    [ text <|
        case s of
            Todo ->
                qmark

            Incorrect ->
                cross

            Correct ->
                tick
    ]


enterSeed : Html Msg
enterSeed =
    div [] [ text "seed = ", input [ type_ "number", name "seed", onInput NewSeed ] [] ]


fv v =
    case v of
        Nothing ->
            ""

        Just i ->
            String.fromInt i


questionHtml : Question -> Int -> List (Html Msg)
questionHtml (Division q) row =
    [ divideText q |> text
    , div [class "answer"]
        [ input [ value (fv q.quotient), type_ "number", name "quotient", onInput (\x -> UpdateQuestion (Quotient row x)) ] []
        , text " remainder "
        , input [ value (fv q.remainder), type_ "number", name "remainder", onInput (\x -> UpdateQuestion (Remainder row x)) ] []
        ]
    ]


divideText d =
    String.fromInt d.dividend ++ divideSymbol ++ String.fromInt d.divisor ++ " = "


randomQuestion : Random.Generator Question
randomQuestion =
    Random.map3
        (\x y z ->
            Division { divisor = x, dividend = x * y + z, quotient = Nothing, remainder = Nothing }
        )
        (Random.int 3 17)
        (Random.int 7 250)
        (Random.int 0 17)


randomQuestions : Random.Generator (List Question)
randomQuestions =
    Random.list 10 randomQuestion


cheat =
    button [ onClick Cheat ] [ text "CHEAT!" ]


view : Model -> Html Msg
view model =
    enterSeed
        :: List.map rowHtml model.rows
        ++ [cheat ]
        |> List.intersperse (hr [] [])
        |> div []


updateRow : Int -> (Question -> Question) -> (Row -> Row)
updateRow i f =
    \r ->
        if r.index == i then
            let
                q =
                    f r.question
            in
            { r | question = q, status = checkQuestion q }

        else
            r


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewSeed s ->
            ( modelFromSeed <| String.toInt s, Cmd.none )

        Cheat ->
            ( { model | rows = List.map showAnswers model.rows }, Cmd.none )

        UpdateQuestion qm ->
            let
                _ =
                    Debug.log "message is" msg

                f : Row -> Row
                f =
                    case qm of
                        Quotient row data ->
                            updateRow row (updateQuotient (String.toInt data))

                        Remainder row data ->
                            updateRow row (updateRemainder (String.toInt data))

                _ =
                    Debug.log "model = " (List.map f model.rows)
            in
            ( { model | rows = List.map f model.rows }, Cmd.none )


initModel =
    makeRows []


modelFromSeed : Maybe Int -> Model
modelFromSeed s =
    let
        seed =
            Maybe.withDefault 0 s
    in
    { rows = makeRows <| randomValuesFromSeed seed, seed = seed }


randomValuesFromSeed : Int -> List Question
randomValuesFromSeed i =
    Random.step randomQuestions (Random.initialSeed i) |> Tuple.first


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( modelFromSeed <| Just 0, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }

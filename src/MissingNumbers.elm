module MissingNumbers exposing (main)

import Html exposing (input, text)
import Html.Attributes exposing (class, name, type_, value)
import Html.Events exposing (onInput)
import Question
import Random


type Msg
    = Value String


type MissingNumber
    = MissingMultiplicand Int Int (Maybe Int)
    | MissingMultiplier Int Int (Maybe Int)
    | MissingProduct Int Int (Maybe Int)


type
    QType
    -- abc -> a x b = c where X is the missing value
    = NMX
    | NXP
    | XMP
    | MNX
    | MXP
    | XNP


multSymbol =
    Question.unicode 0xD7


generator : Int -> List Int -> Random.Generator MissingNumber
generator number numbers =
    Random.map3
        (\type_ m n ->
            case type_ of
                XMP ->
                    MissingMultiplicand m (n * m) Nothing

                XNP ->
                    MissingMultiplicand n (n * m) Nothing

                MXP ->
                    MissingMultiplier m (n * m) Nothing

                NXP ->
                    MissingMultiplier n (n * m) Nothing

                NMX ->
                    MissingProduct n m Nothing

                MNX ->
                    MissingProduct m n Nothing
        )
        (Random.uniform NMX [ NXP, XMP, MNX, MXP, XNP ])
        (Random.int 1 12)
        (Random.uniform number numbers)


solve : MissingNumber -> MissingNumber
solve x =
    case x of
        MissingMultiplicand n p _ ->
            MissingMultiplicand n p (Just (p // n))

        MissingMultiplier n p _ ->
            MissingMultiplier n p (Just (p // n))

        MissingProduct n m _ ->
            MissingProduct n m (Just (n * m))


view : MissingNumber -> Question.Convert Msg -> Question.Qhtml Msg
view x f =
    let
        ( ( a, b, c ), d ) =
            case x of
                MissingMultiplicand n p m ->
                    ( ( "?", String.fromInt n, String.fromInt p ), m )

                MissingMultiplier m p n ->
                    ( ( String.fromInt m, "?", String.fromInt p ), n )

                MissingProduct n m p ->
                    ( ( String.fromInt n, String.fromInt m, "?" ), p )
    in
    { question = text <| a ++ multSymbol ++ b ++ " = " ++ c
    , answer =
        input
            [ value (toString d)
            , type_ "number"
            , name "missing"
            , onInput (\i -> f (Value i))
            ]
            []
    }


toString v =
    case v of
        Nothing ->
            ""

        Just i ->
            String.fromInt i


update : MissingNumber -> Msg -> MissingNumber
update x (Value s) =
    let
        i =
            String.toInt s
    in
    case x of
        MissingMultiplicand a b _ ->
            MissingMultiplicand a b i

        MissingMultiplier a b _ ->
            MissingMultiplier a b i

        MissingProduct a b _ ->
            MissingProduct a b i


main : Program String (Question.Model MissingNumber) (Question.Msg Msg)
main =
    Question.element
        { generator = generator 2 [ 3, 4, 5, 6, 9 ]
        , solve = solve
        , view = view
        , update = update
        , repeats = 15
        }

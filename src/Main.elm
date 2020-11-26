module Main exposing (..)

import Binary exposing (fromDecimal, toIntegers, toString)
import Browser
import Html exposing (Html, button, div, input, label, li, p, span, text, ul)
import Html.Attributes exposing (class, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import List exposing (filter, filterMap, indexedMap, sum)
import List.Extra exposing (removeAt, setAt)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { inputs : List String
    , target : String
    , epsilon : String
    , solution : Maybe (List Float)
    }


init : Model
init =
    { inputs = [ "5", "6", "3.5", "4" ]
    , target = "13.5"
    , epsilon = "0.005"
    , solution = Nothing
    }
        |> (\m -> { m | solution = findSolution m })


type Msg
    = AddInput
    | RemoveInput Int
    | EditInput Int String
    | EditEpsilon String
    | EditTarget String


update : Msg -> Model -> Model
update msg model =
    (case msg of
        AddInput ->
            { model | inputs = "0" :: model.inputs }

        RemoveInput idx ->
            { model | inputs = removeAt idx model.inputs }

        EditInput idx value ->
            { model | inputs = setAt idx value model.inputs }

        EditEpsilon epsilon ->
            { model | epsilon = epsilon }

        EditTarget target ->
            { model | target = target }
    )
        |> (\m -> { m | solution = findSolution m })


view : Model -> Html Msg
view model =
    div [ class "main-content" ]
        [ span []
            [ label [] [ text "Objetivo ($)" ]
            , input [ type_ "number", value model.target, onInput EditTarget ] []
            ]
        , span []
            [ label [] [ text "Margen de Error ($)" ]
            , input [ type_ "number", value model.epsilon, onInput EditEpsilon ] []
            ]
        , span [] [ button [ onClick AddInput ] [ text "Agregar Entrada" ] ]
        , ul [ class "input-values" ] (indexedMap inputItem model.inputs)
        , viewSolution model.solution
        ]


inputItem : Int -> String -> Html Msg
inputItem idx v =
    li []
        [ text "$"
        , input
            [ type_ "number"
            , value v
            , onInput (\nv -> EditInput idx nv)
            ]
            []
        , button [ onClick (RemoveInput idx) ] [ text "X" ]
        ]


viewSolution : Maybe (List Float) -> Html Msg
viewSolution solution =
    case solution of
        Just values ->
            div [ class "s solution" ]
                [ text "Se encontró una solución"
                , ul [] (List.map viewValue values)
                ]

        Nothing ->
            div [ class "s no-solution" ] [ text "No se encontró solución" ]


viewValue : Float -> Html Msg
viewValue value =
    li [] [ text ("$ " ++ String.fromFloat value) ]


findSolution : Model -> Maybe (List Float)
findSolution model =
    let
        inputs =
            filterMap String.toFloat model.inputs |> filter ((/=) 0)

        go i target epsilon =
            if i == 0 then
                Nothing

            else
                let
                    acceptedInputs =
                        Binary.fromDecimal i
                            |> Binary.toBooleans
                            |> padLeft (List.length inputs) False
                            |> List.map2 Tuple.pair inputs
                            |> filterMap takeOnBitEnabled

                    acceptedInputsSum =
                        sum acceptedInputs
                in
                if abs (acceptedInputsSum - target) < epsilon then
                    Just acceptedInputs

                else
                    go (i - 1) target epsilon
    in
    case ( String.toFloat model.target, String.toFloat model.epsilon ) of
        ( Just target, Just epsilon ) ->
            go (2 ^ List.length inputs) target epsilon

        _ ->
            Nothing



-- Utilities


takeOnBitEnabled : ( a, Bool ) -> Maybe a
takeOnBitEnabled ( value, bit ) =
    if bit then
        Just value

    else
        Nothing


padLeft : Int -> a -> List a -> List a
padLeft targetLength pad list =
    let
        listLength =
            List.length list
    in
    if listLength >= targetLength then
        list

    else
        List.repeat (targetLength - listLength) pad ++ list

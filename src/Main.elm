module Main exposing (..)

import Html exposing (h1, div, Html)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Animation
import Color exposing (black, rgb, red)


type alias Model =
    { style : Animation.State
    , index : Int
    }


type Msg
    = Morph
    | Animate Animation.Msg


type alias Slice =
    List Animation.PathStep


initialModel : Model
initialModel =
    { style =
        Animation.style
            [ Animation.stroke red
            , Animation.strokeWidth 2
            , Animation.path slice1
            ]
    , index = 1
    }



-- <path d="M 500 500 L  500 0 A 500 500 0 0 1 975.5282581475768 654.5084971874735 Z" stroke="transparent" fill="#4D4D4D"></path>


slice1 : Slice
slice1 =
    [ Animation.moveTo 500 500
    , Animation.lineTo 1000 500
    , Animation.arc simpleArc
    , Animation.close
    ]


simpleArc : Animation.Arc
simpleArc =
    { x = 500
    , y = 500
    , radius = 500
    , startAngle = 0
    , endAngle = 45
    , clockwise = True
    }


slice2 : Slice
slice2 =
    [ Animation.moveTo 500 500
    , Animation.lineTo 1000 500
    , Animation.arc simpleArc2
    , Animation.close
    ]


simpleArc2 : Animation.Arc
simpleArc2 =
    { x = 500
    , y = 500
    , radius = 500
    , startAngle = 0
    , endAngle = 90
    , clockwise = True
    }


slices : List Slice
slices =
    [ slice1
    , slice2
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Morph ->
            let
                wrappedIndex =
                    if List.length slices < model.index then
                        model.index - List.length slices
                    else
                        model.index

                newPath =
                    Maybe.withDefault slice1 <|
                        List.head <|
                            (List.drop wrappedIndex slices)
                                ++ (List.take wrappedIndex slices)
            in
                { model
                    | index = wrappedIndex + 1
                    , style =
                        Animation.interrupt
                            [ Animation.to
                                [ Animation.path newPath ]
                            ]
                            model.style
                }
                    ! []

        Animate time ->
            { model
                | style = Animation.update time model.style
            }
                ! []


view : Model -> Html Msg
view model =
    div
        [ onClick Morph
        , Attr.style [ ( "margin", "200px auto" ), ( "width", "1000px" ), ( "height", "1000px" ), ( "cursor", "pointer" ) ]
        ]
        [ h1 [] [ text "Click to morph!" ]
        , svg
            [ version "1.1"
            , x "0"
            , y "0"
            , viewBox "0 0 1000 1000"
            , width "1000"
            ]
            [ Svg.path (Animation.render model.style) []
            ]
        ]


init : ( Model, Cmd Msg )
init =
    initialModel ! []


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\model -> Animation.subscription Animate [ model.style ])
        }

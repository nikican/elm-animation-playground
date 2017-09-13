module Main exposing (..)

import Time exposing (second)
import Html exposing (h1, div, Html)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Animation exposing (px)
import Color exposing (purple, green, rgb)


type alias Model =
    { slices : List Animation.State
    }


type Msg
    = EverybodySwitch
    | Animate Animation.Msg


type alias Slice =
    List Animation.Property


slice1 : Slice
slice1 =
    [ Animation.fill Color.red
    , Animation.path <| slicePath 500 500 500 0 70
    ]


slice2 : Slice
slice2 =
    [ Animation.fill Color.blue
    , Animation.path <| slicePath 500 500 500 (0 + 360 * 30 / 100) 70
    ]


slice3 : Slice
slice3 =
    [ Animation.fill Color.red
    , Animation.path <| slicePath 500 500 500 20 40
    ]


slice4 : Slice
slice4 =
    [ Animation.fill Color.blue
    , Animation.path <| slicePath 500 500 500 (0 + 360 * 50 / 100) 50
    ]


init : ( Model, Cmd Msg )
init =
    { slices =
        let
            _ =
                Debug.log "slice1" slice1
        in
            List.map Animation.style [ slice1 ]
    }
        ! []


newSlices : List Slice
newSlices =
    [ slice3 ]


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        EverybodySwitch ->
            { model
                | slices =
                    List.map3
                        (\i slice newStyle ->
                            Animation.interrupt
                                [ Animation.wait (toFloat i * 1 * second)
                                , Animation.to newStyle
                                ]
                                slice
                        )
                        (List.range 0 (List.length model.slices))
                        model.slices
                        newSlices
            }
                ! []

        Animate time ->
            { model
                | slices = List.map (Animation.update time) model.slices
            }
                ! []


view : Model -> Html Msg
view model =
    div
        [ onClick EverybodySwitch
        , Attr.style [ ( "margin", "200px auto" ), ( "width", "1000px" ), ( "height", "1000px" ), ( "cursor", "pointer" ) ]
        ]
        [ h1 [] [ text "Click to morph!" ]
        , svg
            [ version "1.1"
            , x "0"
            , y "0"
            , viewBox "0 0 1000 1000"

            -- , Svg.Attributes.style "transform: rotate(-90deg)"
            ]
          <|
            drawSlice model.slices
        ]


drawSlice : List Animation.State -> List (Svg msg)
drawSlice slices =
    List.map (\slice -> Svg.path (Animation.render slice) []) slices


slicePath : Float -> Float -> Float -> Float -> Float -> List Animation.PathStep
slicePath x y radius startAngle percent =
    let
        dx =
            radius * cos (degrees startAngle)

        dy =
            radius * sin (degrees startAngle)

        endAngle =
            startAngle + 360 * percent / 100
    in
        [ Animation.moveTo x y
        , Animation.lineTo (x + dx) (y + dy)
        , Animation.arc
            { x = x
            , y = y
            , radius = radius
            , startAngle = startAngle
            , endAngle = endAngle
            , clockwise = True
            }
        , Animation.close
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions =
            (\model ->
                Animation.subscription
                    Animate
                    model.slices
            )
        }

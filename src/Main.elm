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


type alias Palette =
    { orange : Color.Color
    , green : Color.Color
    , lavender : Color.Color
    , blue : Color.Color
    }


palette : Palette
palette =
    { orange = rgb 240 173 0
    , green = rgb 127 209 59
    , lavender = rgb 90 99 120
    , blue = rgb 96 181 204
    }


type alias Slice =
    List Animation.Property


slice1 : Slice
slice1 =
    [ Animation.fill palette.green
    , Animation.path
        [ Animation.moveTo 500 500
        , Animation.lineTo 1000 500
        , Animation.arc
            { x = 500
            , y = 500
            , radius = 500
            , startAngle = 0
            , endAngle = 45
            , clockwise = True
            }
        , Animation.close
        ]
    ]


slice2 : Slice
slice2 =
    [ Animation.fill palette.blue
    , Animation.path
        [ Animation.moveTo 500 500
        , Animation.lineTo 1000 500
        , Animation.arc
            { x = 500
            , y = 500
            , radius = 500
            , startAngle = 0
            , endAngle = 180
            , clockwise = True
            }
        , Animation.close
        ]
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        EverybodySwitch ->
            let
                newSlices =
                    [ slice2 ]
            in
                ( { model
                    | slices =
                        List.map3
                            (\i slice newStyle ->
                                Animation.interrupt
                                    [ Animation.wait (toFloat i * 0.05 * second)
                                    , Animation.to newStyle
                                    ]
                                    slice
                            )
                            (List.range 0 (List.length model.slices))
                            model.slices
                            newSlices
                  }
                , Cmd.none
                )

        Animate time ->
            ( { model
                | slices = List.map (Animation.update time) model.slices
              }
            , Cmd.none
            )


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


init : ( Model, Cmd Msg )
init =
    ( { slices =
            let
                radius =
                    500

                startAngle =
                    0

                x =
                    500

                y =
                    500
            in
                [ Animation.style
                    [ Animation.fill Color.red
                    , Animation.strokeWidth 2
                    , Animation.stroke Color.red
                    , Animation.path <| slicePath x y radius startAngle 30

                    -- , Animation.path <|
                    --     [ Animation.moveTo x y
                    --     , Animation.lineTo (x + dx) (y + dy)
                    --     , Animation.arc
                    --         { x = x
                    --         , y = y
                    --         , radius = radius
                    --         , startAngle = startAngle
                    --         , endAngle = endAngle
                    --         , clockwise = True
                    --         }
                    --     , Animation.close
                    --     ]
                    ]
                ]

      --List.map Animation.style slices
      }
    , Cmd.none
    )


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

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
    { styles : List Animation.State
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
            , endAngle = 360
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
            , endAngle = 360
            , clockwise = True
            }
        , Animation.close
        ]
    ]


slices : List (List Animation.Property)
slices =
    [ slice1 ]



-- , [ Animation.fill palette.green
--   , slice2
--   ]


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        EverybodySwitch ->
            let
                newStyles =
                    [ slice2 ]
            in
                ( { model
                    | styles =
                        List.map3
                            (\i style newStyle ->
                                Animation.interrupt
                                    [ Animation.wait (toFloat i * 0.05 * second)
                                    , Animation.to newStyle
                                    ]
                                    style
                            )
                            (List.range 0 (List.length model.styles))
                            model.styles
                            newStyles
                  }
                , Cmd.none
                )

        Animate time ->
            ( { model
                | styles = List.map (Animation.update time) model.styles
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
            ]
          <|
            (List.map (\slice -> Svg.path (Animation.render slice) []) model.styles)
        ]


init : ( Model, Cmd Msg )
init =
    ( { styles =
            --initial slice with really really small arc so that first added item animates
            [ Animation.style
                [ Animation.fill palette.orange
                , Animation.path
                    [ Animation.moveTo 500 500
                    , Animation.lineTo 1000 500
                    , Animation.arc
                        { x = 500
                        , y = 500
                        , radius = 500
                        , startAngle = 0
                        , endAngle = 0.00000001
                        , clockwise = True
                        }
                    , Animation.close
                    ]
                ]
            ]

      --List.map Animation.style slices
      }
    , Cmd.none
    )


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
                    model.styles
            )
        }

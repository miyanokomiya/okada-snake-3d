module Pointer exposing (onTouchEndWithPosition)

import Browser.Events exposing (onAnimationFrameDelta)
import Draggable
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Html.Events
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Json.Decode as Decode exposing (Decoder)


onTouchEndWithPosition : (( Float, Float ) -> msg) -> Html.Attribute msg
onTouchEndWithPosition a =
    onTouchEnd (touchCoordinates >> a)


onTouchEnd : (EventWithOffset -> msg) -> Html.Attribute msg
onTouchEnd tag =
    let
        decoder =
            decodeEventWithOffset
                |> Decode.map tag
                |> Decode.map (\msg -> { message = msg, preventDefault = False, stopPropagation = False })
    in
    Html.Events.custom "touchend" decoder


touchCoordinates : EventWithOffset -> ( Float, Float )
touchCoordinates { event, targetOffset } =
    List.head event.changedTouches
        |> Maybe.map (.pagePos >> fromCoordinates targetOffset)
        |> Maybe.withDefault ( 0, 0 )


fromCoordinates : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
fromCoordinates ( x0, y0 ) ( x, y ) =
    ( x - x0, y - y0 )


type alias EventWithOffset =
    { event : Touch.Event
    , targetOffset : ( Float, Float )
    }


decodeEventWithOffset : Decoder EventWithOffset
decodeEventWithOffset =
    Decode.map2 EventWithOffset
        Touch.eventDecoder
        offsetDecoder


offsetDecoder : Decoder ( Float, Float )
offsetDecoder =
    Decode.field "target" <|
        Decode.map2 (\a b -> ( a, b ))
            (Decode.field "offsetLeft" Decode.float)
            (Decode.field "offsetTop" Decode.float)

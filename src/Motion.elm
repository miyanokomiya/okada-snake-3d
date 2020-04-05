module Motion exposing
    ( GeoAnimation
    , PositionAnimation
    , animateGeo
    , animatePosition
    , animateRotate
    , positionAnimation
    , repeatAnimation
    , rotateAnimation
    , staticPositionAnimation
    , staticRotateAnimation
    )

import Animation exposing (Animation)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Shader


repeatAnimation : Animation.Clock -> Animation -> Animation
repeatAnimation clock animation =
    if Animation.isDone clock animation then
        animation
            |> Animation.delay (Animation.getDuration animation + Animation.getDelay animation)

    else
        animation


rotateAnimation : Float -> Float -> Float -> Float -> Animation
rotateAnimation clock duration from to =
    Animation.animation clock
        |> Animation.from from
        |> Animation.to to
        |> Animation.duration duration
        |> Animation.delay 0
        |> Animation.ease (\x -> x)


staticRotateAnimation : Float -> Animation
staticRotateAnimation current =
    Animation.static current


animateRotate : Float -> Animation -> Shader.Geo -> Shader.Geo
animateRotate t animation geo =
    let
        rotation =
            geo.rotation

        next =
            Animation.animate t animation
    in
    { geo | rotation = { rotation | radian = next } }


type alias PositionAnimation =
    { x : Animation
    , y : Animation
    , z : Animation
    }


type alias GeoAnimation =
    { position : PositionAnimation
    , rotation : Animation
    }


positionAnimation : Animation.Clock -> Float -> Vec3 -> Vec3 -> PositionAnimation
positionAnimation clock duration from to =
    let
        anim =
            Animation.animation clock
                |> Animation.duration duration
                |> Animation.delay 0
                |> Animation.ease (\x -> x)
    in
    { x = anim |> Animation.from (Vec3.getX from) |> Animation.to (Vec3.getX to)
    , y = anim |> Animation.from (Vec3.getY from) |> Animation.to (Vec3.getY to)
    , z = anim |> Animation.from (Vec3.getZ from) |> Animation.to (Vec3.getZ to)
    }


staticPositionAnimation : Vec3 -> PositionAnimation
staticPositionAnimation vec =
    { x = Animation.static (Vec3.getX vec)
    , y = Animation.static (Vec3.getY vec)
    , z = Animation.static (Vec3.getZ vec)
    }


animatePosition : Float -> PositionAnimation -> Shader.Geo -> Shader.Geo
animatePosition t animation geo =
    let
        next =
            vec3 (Animation.animate t animation.x) (Animation.animate t animation.y) (Animation.animate t animation.z)
    in
    { geo | position = next }


animateGeo : Float -> GeoAnimation -> Shader.Geo -> Shader.Geo
animateGeo t anim current =
    current
        |> animateRotate t anim.rotation
        |> animatePosition t anim.position

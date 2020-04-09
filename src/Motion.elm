module Motion exposing
    ( GeoAnimation
    , PositionAnimation
    , animatePosition
    , animatePositionVec
    , animateRotate
    , positionAnimation
    , repeatAnimation
    , rotateAnimation
    , staticPositionAnimation
    , staticRotateAnimation
    )

import Animation exposing (Animation)
import Math.Matrix4 as Mat4 exposing (Mat4)
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


animatePosition : Float -> PositionAnimation -> Mat4
animatePosition t animation =
    Mat4.makeTranslate (animatePositionVec t animation)


animatePositionVec : Float -> PositionAnimation -> Vec3
animatePositionVec t animation =
    vec3 (Animation.animate t animation.x) (Animation.animate t animation.y) (Animation.animate t animation.z)

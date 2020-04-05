module Shader exposing
    ( Geo
    , OrbitCamela
    , Rotation
    , Triangle
    , Vertex
    , fragmentShader
    , getClickPosition
    , isMeshClicked
    , getClickedMesh
    , orbitCamelaPosition
    , rotationToMat
    , uniforms
    , vertexShader
    )

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4
import WebGL exposing (Shader)


type alias Triangle =
    ( Vec3, Vec3, Vec3 )


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


type alias Rotation =
    { radian : Float
    , axis : Vec3
    }


type alias Geo =
    { position : Vec3
    , rotation : Rotation
    }


{-| state of OrbitCamela

Parameters:

  - radius - radius from origin
  - radianY - radian by axis Y
  - radianZ - radian by axis Z

-}
type alias OrbitCamela =
    ( Float, Float, Float )


type alias Uniforms =
    { rotation : Mat4
    , translation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    }


rotationToMat : Rotation -> Mat4
rotationToMat r =
    Mat4.makeRotate r.radian r.axis


uniforms : OrbitCamela -> Mat4 -> Vec3 -> Mat4 -> Uniforms
uniforms camera perspective position rotation =
    { rotation = rotation
    , translation = Mat4.makeTranslate position
    , perspective = perspective
    , camera = cameraLootAk camera
    , shade = 0.8
    }


cameraLootAk : OrbitCamela -> Mat4
cameraLootAk camera =
    Mat4.makeLookAt (orbitCamelaPosition camera) (vec3 0 0 0) (vec3 0 1 0)


orbitCamelaPosition : OrbitCamela -> Vec3
orbitCamelaPosition ( cr, ca, cb ) =
    Mat4.transform
        (Mat4.mul
            (Mat4.makeRotate ca (vec3 0 1 0))
            (Mat4.makeRotate cb (vec3 1 0 0))
        )
        (vec3 0 0 cr)



-- Shaders


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        uniform mat4 translation;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * camera * translation * rotation * vec4(position, 1.0);
            vcolor = color;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        uniform float shade;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = shade * vec4(vcolor, 1.0);
        }

    |]


isMeshClicked : Vec3 -> Vec3 -> List Triangle -> Bool
isMeshClicked origin direction list =
    let
        intersect =
            rayTriangleIntersect origin direction
    in
    List.any
        (\triangle ->
            intersect triangle
                |> (\m ->
                        case m of
                            Nothing ->
                                False

                            Just _ ->
                                True
                   )
        )
        list


getClickedMesh : Vec3 -> Vec3 -> List ( List Triangle, a ) -> Maybe a
getClickedMesh origin direction list =
    list
        |> getClickedMeshList origin direction
        |> sortByScaler origin
        |> List.map (\( _, triangles ) -> triangles)
        |> List.head


getClickedMeshList : Vec3 -> Vec3 -> List ( List Triangle, a ) -> List ( Vec3, a )
getClickedMeshList origin direction list =
    let
        intersect =
            rayTriangleIntersect origin direction
    in
    list
        |> List.filterMap
            (\( triangles, a ) ->
                clickedOnTriangles origin intersect triangles
                    |> Maybe.map (\( v, _ ) -> ( v, a ))
            )


clickedOnTriangles : Vec3 -> (Triangle -> Maybe Vec3) -> List Triangle -> Maybe ( Vec3, List Triangle )
clickedOnTriangles origin intersect triangles =
    triangles
        |> List.filterMap
            (\triangle -> intersect triangle)
        |> List.map (\v -> ( v, triangles ))
        |> sortByScaler origin
        |> List.head


sortByScaler : Vec3 -> List ( Vec3, a ) -> List ( Vec3, a )
sortByScaler origin list =
    list
        |> List.sortBy (\( v, _ ) -> Vec3.distance origin v)


rayTriangleIntersect : Vec3 -> Vec3 -> Triangle -> Maybe Vec3
rayTriangleIntersect rayOrigin rayDirection ( triangle0, triangle1, triangle2 ) =
    let
        epsilon =
            0.000001

        edge1 =
            Vec3.sub triangle1 triangle0

        edge2 =
            Vec3.sub triangle2 triangle0

        pvec =
            Vec3.cross rayDirection edge2

        det =
            Vec3.dot edge1 pvec
    in
    if det < epsilon then
        Nothing

    else
        let
            tvec =
                Vec3.sub rayOrigin triangle0

            u =
                Vec3.dot tvec pvec
        in
        if u < 0 || u > det then
            Nothing

        else
            let
                qvec =
                    Vec3.cross tvec edge1

                v =
                    Vec3.dot rayDirection qvec
            in
            if v < 0 || u + v > det then
                Nothing

            else
                let
                    t =
                        Vec3.dot edge2 qvec / det

                    v0 =
                        Vec3.getX rayOrigin + t * Vec3.getX rayDirection

                    v1 =
                        Vec3.getY rayOrigin + t * Vec3.getY rayDirection

                    v2 =
                        Vec3.getZ rayOrigin + t * Vec3.getZ rayDirection
                in
                Just (vec3 v0 v1 v2)


{-| camela -> perspective -> (0 ~ 1, 0 ~ 1) -> vec3
-}
getClickPosition : OrbitCamela -> Mat4 -> ( Float, Float ) -> Vec3
getClickPosition camera perspective ( x, y ) =
    let
        normalizedPosition =
            ( x, y )

        homogeneousClipCoordinates =
            Vec4.vec4
                (Tuple.first normalizedPosition)
                (Tuple.second normalizedPosition)
                -1
                1

        invertedViewMatrix =
            Mat4.inverseOrthonormal (cameraLootAk camera)

        invertedProjectionMatrix =
            Maybe.withDefault Mat4.identity (Mat4.inverse perspective)

        vec4CameraCoordinates =
            mulVector invertedProjectionMatrix homogeneousClipCoordinates

        direction =
            Vec4.vec4 (Vec4.getX vec4CameraCoordinates) (Vec4.getY vec4CameraCoordinates) -1 0

        vec4WorldCoordinates =
            mulVector invertedViewMatrix direction

        vec3WorldCoordinates =
            vec3 (Vec4.getX vec4WorldCoordinates) (Vec4.getY vec4WorldCoordinates) (Vec4.getZ vec4WorldCoordinates)

        normalizedVec3WorldCoordinates =
            Vec3.normalize vec3WorldCoordinates

        origin =
            orbitCamelaPosition camera

        scaledDirection =
            Vec3.scale 20 normalizedVec3WorldCoordinates

        destination =
            Vec3.add origin scaledDirection
    in
    destination


mulVector : Mat4 -> Vec4.Vec4 -> Vec4.Vec4
mulVector mat v =
    let
        rec =
            Mat4.toRecord mat

        r1 =
            Vec4.vec4 rec.m11 rec.m12 rec.m13 rec.m14

        r2 =
            Vec4.vec4 rec.m21 rec.m22 rec.m23 rec.m24

        r3 =
            Vec4.vec4 rec.m31 rec.m32 rec.m33 rec.m34

        r4 =
            Vec4.vec4 rec.m41 rec.m42 rec.m43 rec.m44
    in
    Vec4.vec4 (Vec4.dot r1 v) (Vec4.dot r2 v) (Vec4.dot r3 v) (Vec4.dot r4 v)

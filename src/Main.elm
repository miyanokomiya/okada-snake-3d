module Main exposing (main)

import Animation exposing (Animation)
import Asset
import Block
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Draggable
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Html.Events
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Json.Decode as Decode exposing (Decoder)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Motion
import Pointer
import Random
import Random.Extra
import Round
import Shader
import WebGL exposing (Mesh)


type Okada
    = Oka
    | Da


type alias GeoBlock =
    { id : Int
    , okada : Okada
    , geo : Shader.Geo
    , turning : Shader.Rotation
    , rotateAnimation : Animation
    , positionAnimation : Motion.PositionAnimation
    }


type alias OkadaPair =
    ( GeoBlock, GeoBlock )


type alias ColorPair =
    ( Vec3, Vec3 )


blockTransFormMat : GeoBlock -> Mat4
blockTransFormMat block =
    let
        p =
            block.geo.position

        r =
            block.geo.rotation
    in
    Mat4.mul (Mat4.makeTranslate p) (Mat4.mul (Shader.rotationToMat r) (Shader.rotationToMat block.turning))


animateGeoBlock : Float -> GeoBlock -> GeoBlock
animateGeoBlock t current =
    let
        rotated =
            Motion.animateRotate t current.rotateAnimation current.geo

        nextGeo =
            Motion.animatePosition t current.positionAnimation rotated

        nextTurning =
            { radian = current.turning.radian + pi / 300, axis = current.turning.axis }
    in
    { current | geo = nextGeo, turning = nextTurning }


defaultColor : ColorPair
defaultColor =
    ( vec3 60 179 113, vec3 47 79 79 )


selectedColor : ColorPair
selectedColor =
    ( vec3 165 42 42, vec3 204 0 153 )


completedColor : ColorPair
completedColor =
    ( vec3 255 215 0, vec3 255 165 0 )


type alias MeshSet =
    { oka : Mesh Shader.Vertex
    , da : Mesh Shader.Vertex
    }


type alias Model =
    { time : Float
    , completedTime : Float
    , size : ( Int, Int )
    , level : Int
    , camera : Shader.OrbitCamela
    , downTime : Float
    , drag : Draggable.State String
    , blocks : List GeoBlock
    , pairs : List OkadaPair
    , selected : Maybe GeoBlock
    , meshMap :
        { default : MeshSet
        , selected : MeshSet
        , completed : MeshSet
        }
    }


spreadBlock : Float -> GeoBlock -> Random.Generator GeoBlock
spreadBlock radius block =
    Random.map
        (\geo ->
            { block
                | positionAnimation = Motion.positionAnimation 0 200 block.geo.position geo.position
                , rotateAnimation = Motion.staticRotateAnimation geo.rotation.radian
                , geo = { position = block.geo.position, rotation = geo.rotation }
            }
        )
        (randomGeo radius)


randomGeo : Float -> Random.Generator Shader.Geo
randomGeo radius =
    Random.map3
        (\rad axis pos -> { position = pos, rotation = { radian = rad, axis = axis } })
        (Random.float 0 (2 * pi))
        (randomVec3 1)
        (randomVec3 radius)


randomVec3 : Float -> Random.Generator Vec3
randomVec3 radius =
    Random.map3
        (\x y z -> vec3 x y z)
        (Random.float -radius radius)
        (Random.float -radius radius)
        (Random.float -radius radius)


type Msg
    = Reset
    | Spread (List GeoBlock)
    | Next
    | Delta Float
    | OnDragBy Draggable.Delta
    | DragMsg (Draggable.Msg String)
    | ClickMsg ( Float, Float )


dragConfig : Draggable.Config String Msg
dragConfig =
    Draggable.basicConfig OnDragBy


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            initModel 1
    in
    ( model
    , generateSpreadCmd model
    )


countAtLevel : Int -> Int
countAtLevel level =
    2 ^ level


cameraRadius : Int -> Float
cameraRadius level =
    4 + toFloat (4 * level)


initModel : Int -> Model
initModel level =
    { time = 0
    , completedTime = 0
    , size = ( 400, 600 )
    , level = level
    , camera = ( cameraRadius level, pi / 8, pi / 16 )
    , downTime = 0
    , drag = Draggable.init
    , blocks =
        let
            count =
                countAtLevel level
        in
        List.range 1 count
            |> List.map
                (\i ->
                    let
                        okada =
                            if modBy 2 i == 0 then
                                Oka

                            else
                                Da

                        geo =
                            { position = vec3 0 0 0, rotation = { radian = 0, axis = vec3 1 0 0 } }
                    in
                    { id = i
                    , okada = okada
                    , geo = geo
                    , turning = { radian = 0, axis = vec3 0 1 0 }
                    , rotateAnimation = Motion.staticRotateAnimation 0
                    , positionAnimation = Motion.staticPositionAnimation (vec3 0 0 0)
                    }
                )
    , pairs = []
    , selected = Nothing
    , meshMap =
        { default = { oka = okadaMesh defaultColor Oka, da = okadaMesh defaultColor Da }
        , selected = { oka = okadaMesh selectedColor Oka, da = okadaMesh selectedColor Da }
        , completed = { oka = okadaMesh completedColor Oka, da = okadaMesh completedColor Da }
        }
    }


okadaMesh : ColorPair -> Okada -> Mesh Shader.Vertex
okadaMesh ( faceColor, sideColor ) okada =
    case okada of
        Oka ->
            Block.meshOka faceColor sideColor

        Da ->
            Block.meshDa faceColor sideColor


limitRadian : Float -> Float
limitRadian r =
    max (min r (pi / 2 * 0.99)) (-pi / 2 * 0.99)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( width, height ) =
            model.size

        ( cr, ca, cb ) =
            model.camera
    in
    case msg of
        Reset ->
            let
                next =
                    initModel 1
            in
            ( next
            , generateSpreadCmd next
            )

        Spread blocks ->
            ( { model | blocks = blocks }
            , Cmd.none
            )

        Next ->
            let
                next =
                    initModel (model.level + 1)
            in
            ( next
            , generateSpreadCmd next
            )

        Delta dt ->
            let
                time =
                    model.time + dt

                completedTime =
                    if isLevelCompleted model then
                        model.completedTime

                    else
                        time

                ( a, b, c ) =
                    model.camera

                camela =
                    if isLevelCompleted model && model.downTime == 0 then
                        ( a, b - (dt / 1000), c )

                    else
                        model.camera
            in
            ( { model
                | time = time
                , completedTime = completedTime
                , camera = camela
                , blocks =
                    model.blocks
                        |> List.map (\block -> animateGeoBlock time block)
                , selected = Maybe.map (\block -> animateGeoBlock time block) model.selected
                , pairs =
                    model.pairs
                        |> List.map
                            (\( blockA, blockB ) ->
                                ( animateGeoBlock time blockA
                                , animateGeoBlock time blockB
                                )
                            )
              }
            , Cmd.none
            )

        OnDragBy ( dx, dy ) ->
            ( { model
                | camera = ( cr, ca - dx / 30, limitRadian (cb - dy / 30) )
                , downTime = model.downTime + 1
              }
            , Cmd.none
            )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model

        ClickMsg ( x, y ) ->
            if model.downTime < 10 then
                let
                    nextModel =
                        clickBlock model (getClickedBlock model ( (x * 2) / toFloat width - 1, 1 - y / toFloat height * 2 ))
                in
                ( { nextModel | downTime = 0 }
                , Cmd.none
                )

            else
                ( { model | downTime = 0 }, Cmd.none )


generateSpreadCmd : Model -> Cmd Msg
generateSpreadCmd model =
    Random.generate Spread (Random.Extra.traverse (\b -> spreadBlock (cameraRadius model.level * 0.2) b) model.blocks)


clickBlock : Model -> Maybe GeoBlock -> Model
clickBlock model maybeBlock =
    case maybeBlock of
        Nothing ->
            model

        Just block ->
            case model.selected of
                Nothing ->
                    { model
                        | selected = maybeBlock
                        , blocks = List.filter (\b -> b.id /= block.id) model.blocks
                    }

                Just current ->
                    if block.id == current.id then
                        { model
                            | selected = Nothing
                            , blocks = model.blocks ++ [ current ]
                        }

                    else
                        let
                            maybePair =
                                createPair block current
                        in
                        case maybePair of
                            Just pair ->
                                let
                                    pairWithAnimation =
                                        makePairAnimation model.time pair
                                in
                                { model
                                    | selected = Nothing
                                    , blocks = List.filter (\b -> b.id /= block.id) model.blocks
                                    , pairs =
                                        List.append model.pairs [ pairWithAnimation ]
                                }

                            Nothing ->
                                model


makePairAnimation : Animation.Clock -> OkadaPair -> OkadaPair
makePairAnimation clock ( blockA, blockB ) =
    let
        posA =
            blockA.geo.position

        posB =
            blockB.geo.position

        center =
            Vec3.add posA posB |> Vec3.scale (1 / 2)

        toA =
            Vec3.add center (vec3 0 0.6 0)

        toB =
            Vec3.add center (vec3 0 -0.6 0)

        map =
            \( b, to ) ->
                { b
                    | positionAnimation = Motion.positionAnimation clock 500 b.geo.position to
                    , rotateAnimation = Motion.rotateAnimation clock 500 b.geo.rotation.radian (pi * 4)
                }
    in
    Tuple.mapBoth map map ( ( blockA, toA ), ( blockB, toB ) )


createPair : GeoBlock -> GeoBlock -> Maybe OkadaPair
createPair a b =
    if a.okada == Oka && b.okada == Da then
        Just ( a, b )

    else if a.okada == Da && b.okada == Oka then
        Just ( b, a )

    else
        Nothing


getClickedBlock : Model -> ( Float, Float ) -> Maybe GeoBlock
getClickedBlock model pos =
    let
        origin =
            Shader.orbitCamelaPosition model.camera

        destination =
            Shader.getClickPosition model.camera (getPerspective model.size) pos

        direction =
            Vec3.direction destination origin
    in
    (model.blocks ++ Maybe.withDefault [] (Maybe.map (\b -> [ b ]) model.selected))
        |> List.map
            (\block ->
                let
                    mat =
                        blockTransFormMat block

                    triangles =
                        List.map (\( v0, v1, v2 ) -> ( Mat4.transform mat v0, Mat4.transform mat v1, Mat4.transform mat v2 )) (List.concat Asset.cube)
                in
                ( triangles, block )
            )
        |> Shader.getClickedMesh origin direction


isLevelCompleted : Model -> Bool
isLevelCompleted model =
    List.length model.blocks == 0


isAllLevelCompleted : Model -> Bool
isAllLevelCompleted model =
    model.level == 8 && isLevelCompleted model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta (\dt -> Delta dt)
        , Draggable.subscriptions DragMsg model.drag
        ]


view : Model -> Browser.Document Msg
view model =
    let
        ( w, h ) =
            model.size

        perspective =
            getPerspective model.size
    in
    { title = "岡田集め - リバイバル"
    , body =
        [ Html.div
            [ style "display" "flex"
            , style "justify-content" "center"
            ]
            [ Html.div
                [ style "width" (String.fromInt w ++ "px")
                ]
                [ Html.div
                    [ style "display" "flex"
                    , style "justify-content" "space-between"
                    , style "padding" "0.1rem 0.5rem"
                    , style "font-size" "1.2rem"
                    , style "background-color" "#333"
                    , style "color" "#fff"
                    ]
                    [ Html.span [] [ Html.text ("Lv. " ++ String.fromInt model.level) ]
                    , Html.span [] [ Html.text (timeText model.completedTime) ]
                    ]
                , Html.div
                    [ width w
                    , height h
                    , Mouse.onClick (.offsetPos >> ClickMsg)
                    , Pointer.onTouchEndWithPosition ClickMsg
                    ]
                    [ WebGL.toHtml
                        ([ width w
                         , height h
                         , style "display" "block"
                         , style "background-color" "#eee"
                         , style "border" "1px solid black"
                         , Draggable.mouseTrigger "my-element" DragMsg
                         ]
                            ++ Draggable.touchTriggers "my-element" DragMsg
                        )
                        (entities
                            model.camera
                            perspective
                            model.meshMap.default
                            model.blocks
                            ++ Maybe.withDefault [] (Maybe.map (\b -> [ entity model.camera perspective model.meshMap.selected b ]) model.selected)
                            ++ entities
                                model.camera
                                perspective
                                model.meshMap.completed
                                (model.pairs
                                    |> List.map
                                        (\( oka, da ) -> [ oka, da ])
                                    |> List.concat
                                )
                        )
                    ]
                , Html.div
                    [ Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "justify-content" "space-between"
                    , Html.Attributes.style "margin-top" "0.2rem"
                    ]
                    [ Html.div []
                        [ button
                            [ Html.Events.onClick Reset
                            ]
                            [ Html.text "RESET" ]
                        ]
                    , Html.div
                        [ Html.Attributes.style "display" "flex"
                        , Html.Attributes.style "align-items" "center"
                        ]
                        ((if isLevelCompleted model then
                            if isAllLevelCompleted model then
                                [ button
                                    [ width 100
                                    , Html.Events.onClick Next
                                    , Html.Attributes.style "margin" "0 1rem"
                                    , Html.Attributes.style "background-color" "gold"
                                    ]
                                    [ Html.text "YOU MUST BE OKADA!!!" ]
                                ]

                            else
                                [ button
                                    [ width 100
                                    , Html.Events.onClick Next
                                    , Html.Attributes.style "margin" "0 1rem"
                                    , Html.Attributes.style "background-color" "blue"
                                    ]
                                    [ Html.text "NEXT" ]
                                ]

                          else
                            []
                         )
                            ++ [ Html.span
                                    [ Html.Attributes.style "min-width" "40px"
                                    , Html.Attributes.style "text-align" "right"
                                    ]
                                    [ Html.text (String.fromInt (List.length model.pairs * 2)) ]
                               , Html.span
                                    [ Html.Attributes.style "margin" "0 0.4rem"
                                    ]
                                    [ Html.text "/" ]
                               , Html.span [] [ Html.text (String.fromInt (countAtLevel model.level)) ]
                               ]
                        )
                    ]
                , Html.div
                    [ style "text-align" "center"
                    ]
                    [ Html.a
                        [ Html.Attributes.href "https://github.com/miyanokomiya/okada-gather"
                        , Html.Attributes.target "_blank"
                        , Html.Attributes.rel "noopener"
                        , style "font-size" "0.8rem"
                        ]
                        [ Html.text "repository" ]
                    ]
                ]
            ]
        ]
    }


timeText : Float -> String
timeText time =
    Round.round 1 (time / 1000)


button : List (Html.Attribute msg) -> List (Html msg) -> Html msg
button attrs children =
    Html.button
        ([ Html.Attributes.style "padding" "0.4rem 0.8rem"
         , Html.Attributes.style "background-color" "#444"
         , Html.Attributes.style "color" "white"
         , Html.Attributes.style "border-radius" "4px"
         , Html.Attributes.style "border" "none"
         ]
            ++ attrs
        )
        children


entities : Shader.OrbitCamela -> Mat4 -> MeshSet -> List GeoBlock -> List WebGL.Entity
entities camera perspective set list =
    List.map
        (\block ->
            entity camera perspective set block
        )
        list


entity : Shader.OrbitCamela -> Mat4 -> MeshSet -> GeoBlock -> WebGL.Entity
entity camera perspective set block =
    let
        p =
            block.geo.position

        r =
            block.geo.rotation
    in
    WebGL.entity
        Shader.vertexShader
        Shader.fragmentShader
        (toMesh set block)
        (Shader.uniforms camera perspective p (Mat4.mul (Shader.rotationToMat r) (Shader.rotationToMat block.turning)))


toMesh : MeshSet -> GeoBlock -> Mesh Shader.Vertex
toMesh set block =
    if block.okada == Oka then
        set.oka

    else
        set.da


getPerspective : ( Int, Int ) -> Mat4
getPerspective ( width, height ) =
    Mat4.makePerspective 45 (toFloat width / toFloat height) 0.01 100

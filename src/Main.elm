module Main exposing (main)

import Array
import Asset
import Block
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Draggable
import Grid exposing (Grid)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Html.Events
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Maybe.Extra
import Motion
import Pointer
import Random
import Shader
import WebGL exposing (Mesh)


type Okada
    = Oka
    | Da


type alias GeoBlock =
    { id : String
    , okada : Okada
    , geo : Shader.Geo
    , turning : Shader.Rotation
    }


type Cell
    = Food Okada
    | Empty


type alias PlayerCell =
    { point : Grid.Point
    , rotation : Shader.Rotation
    , turning : Shader.Rotation
    , positionAnimation : Motion.PositionAnimation
    }


initField : Grid Cell
initField =
    Grid.repeat 3 Empty


type alias Player =
    { head : PlayerCell
    , body : List PlayerCell
    }


type alias ColorPair =
    ( Vec3, Vec3 )


defaultColor : ColorPair
defaultColor =
    ( vec3 60 179 113, vec3 47 79 79 )


playerHeadColor : ColorPair
playerHeadColor =
    ( vec3 236 75 40, vec3 150 49 28 )


playerBodyColor : ColorPair
playerBodyColor =
    ( vec3 230 184 85, vec3 186 147 64 )


type alias MeshSet =
    { oka : Mesh Shader.Vertex
    , da : Mesh Shader.Vertex
    }


type alias Model =
    { time : Float
    , size : ( Int, Int )
    , field : Grid Cell
    , player : Player
    , camera : Shader.OrbitCamela
    , downTime : Float
    , drag : Draggable.State String
    , meshMap :
        { default : MeshSet
        , playerHead : MeshSet
        , playerBody : MeshSet
        , line : Mesh Shader.Vertex
        , hilightLine : Mesh Shader.Vertex
        , moveTargetBox : Mesh Shader.Vertex
        , moveTargetBoxLine : Mesh Shader.Vertex
        , connectorLine : Mesh Shader.Vertex
        }
    }


type MoveTo
    = Xplus
    | Xminus
    | Yplus
    | Yminus
    | Zplus
    | Zminus


type Msg
    = Reset
    | Delta Float
    | OnDragBy Draggable.Delta
    | DragMsg (Draggable.Msg String)
    | Move MoveTo
    | ClickMsg ( Float, Float )
    | Zoom Wheel.Event
    | Spawn Grid.Point


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
    , Cmd.none
    )


cameraRadius : Int -> Float
cameraRadius level =
    4 + toFloat (4 * level)


initModel : Int -> Model
initModel level =
    let
        field =
            initField

        player =
            { head =
                { point = ( 0, 1, 0 )
                , rotation = { radian = 0, axis = vec3 0 1 0 }
                , turning = { radian = 0, axis = vec3 0 1 0 }
                , positionAnimation = Motion.staticPositionAnimation (vec3 0 0 0)
                }
            , body = []
            }
    in
    { time = 0
    , field = field
    , player = player
    , camera = setCameraToHead player { radius = cameraRadius level, radianY = pi * 1.4, radianZ = -pi / 8, position = vec3 0 0 0 }
    , size = ( 400, 600 )
    , downTime = 0
    , drag = Draggable.init
    , meshMap =
        { default = { oka = okadaMesh defaultColor Oka, da = okadaMesh defaultColor Da }
        , playerHead = { oka = okadaMesh playerHeadColor Oka, da = okadaMesh playerHeadColor Da }
        , playerBody = { oka = okadaMesh playerBodyColor Oka, da = okadaMesh playerBodyColor Da }
        , line = Block.meshUnitLine (vec3 0 0 0)
        , hilightLine = Block.meshUnitLine (vec3 255 0 0)
        , moveTargetBox = Block.meshCube
        , moveTargetBoxLine = Block.meshCubeLine
        , connectorLine = Block.meshUnitLine (vec3 200 150 25)
        }
    }


okadaMesh : ColorPair -> Okada -> Mesh Shader.Vertex
okadaMesh ( faceColor, sideColor ) okada =
    case okada of
        Oka ->
            Block.meshOka faceColor sideColor

        _ ->
            Block.meshDa faceColor sideColor


limitRadian : Float -> Float
limitRadian r =
    max (min r (pi / 2 * 0.99)) (-pi / 2 * 0.99)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( width, height ) =
            model.size

        camera =
            model.camera
    in
    case msg of
        Reset ->
            let
                next =
                    initModel 1
            in
            ( next
            , Cmd.none
            )

        Delta dt ->
            let
                time =
                    model.time + dt

                headPosition =
                    pointToPosition model.player.head.point

                animatedCamera =
                    { camera | position = Mat4.transform (Motion.animatePosition time model.player.head.positionAnimation) headPosition }
            in
            ( { model
                | time = time
                , camera = animatedCamera
                , player =
                    { head =
                        let
                            head =
                                model.player.head

                            turning =
                                head.turning
                        in
                        { head | turning = { turning | radian = turning.radian + (dt / 1000) } }
                    , body =
                        model.player.body
                            |> List.map
                                (\pc ->
                                    let
                                        turning =
                                            pc.turning
                                    in
                                    { pc | turning = { turning | radian = turning.radian + (dt / 1000) } }
                                )
                    }
              }
            , Cmd.none
            )

        OnDragBy ( dx, dy ) ->
            ( { model
                | camera = { camera | radianY = camera.radianY - dx / 30, radianZ = limitRadian (camera.radianZ - dy / 30) }
                , downTime = model.downTime + 1
              }
            , Cmd.none
            )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model

        ClickMsg ( x, y ) ->
            if model.downTime < 10 then
                let
                    maybeMoveTo =
                        getClickedMoveTo model ( (x * 2) / toFloat width - 1, 1 - y / toFloat height * 2 )

                    movedModel =
                        case maybeMoveTo of
                            Just moveTo ->
                                moveModel moveTo model

                            Nothing ->
                                model

                    expandedModel =
                        if score movedModel == nextExpand movedModel then
                            { movedModel | field = Grid.expand Empty movedModel.field }

                        else
                            movedModel

                    nextModel =
                        { expandedModel | downTime = 0 }
                in
                ( nextModel
                , if model.player == nextModel.player then
                    Cmd.none

                  else
                    generateSpreadCmd nextModel
                )

            else
                ( { model | downTime = 0 }, Cmd.none )

        Move moveTo ->
            let
                nextModel =
                    moveModel moveTo model
            in
            ( nextModel
            , if model.player == nextModel.player then
                Cmd.none

              else
                generateSpreadCmd nextModel
            )

        Zoom event ->
            let
                radius =
                    camera.radius - (event.deltaY / 100)
            in
            ( { model | camera = { camera | radius = max radius 4 } }
            , Cmd.none
            )

        Spawn point ->
            ( { model | field = spawn point model.player model.field }, Cmd.none )


generateSpreadCmd : Model -> Cmd Msg
generateSpreadCmd model =
    Random.generate Spawn (randomPointGenerator (Grid.length model.field))


randomPointGenerator : Int -> Random.Generator Grid.Point
randomPointGenerator gridSize =
    Random.map3
        (\x y z -> ( x, y, z ))
        (Random.int 0 (gridSize - 1))
        (Random.int 0 (gridSize - 1))
        (Random.int 0 (gridSize - 1))


spawn : Grid.Point -> Player -> Grid Cell -> Grid Cell
spawn point player field =
    let
        maybeCurrentCell =
            Grid.get point field
    in
    case maybeCurrentCell of
        Nothing ->
            field

        Just currentCell ->
            let
                playerPoints =
                    player.head :: player.body |> List.map (\c -> c.point)
            in
            if List.member point playerPoints || currentCell /= Empty then
                field

            else
                let
                    okada =
                        tailNotOkada player
                in
                Grid.set point (Food okada) field


movingTime : Float
movingTime =
    100


moveModel : MoveTo -> Model -> Model
moveModel moveTo model =
    let
        camera =
            model.camera

        ( nextField, nextPlayer ) =
            moveAndEat moveTo model.field model.player

        nextCamera =
            setCameraToHead nextPlayer camera

        nextHead =
            nextPlayer.head

        beforeBodyArray =
            Array.fromList model.player.body

        animatedHead =
            { nextHead
                | positionAnimation =
                    Motion.positionAnimation
                        model.time
                        movingTime
                        (Vec3.sub
                            (pointToPosition model.player.head.point)
                            (pointToPosition nextHead.point)
                        )
                        (vec3 0 0 0)
            }

        animatedBody =
            nextPlayer.body
                |> List.indexedMap
                    (\i next ->
                        let
                            maybeBefore =
                                Array.get i beforeBodyArray
                        in
                        case maybeBefore of
                            Just before ->
                                { next
                                    | positionAnimation =
                                        Motion.positionAnimation
                                            model.time
                                            movingTime
                                            (Vec3.sub
                                                (pointToPosition before.point)
                                                (pointToPosition next.point)
                                            )
                                            (vec3 0 0 0)
                                }

                            Nothing ->
                                next
                    )
    in
    { model
        | downTime = 0
        , field = nextField
        , player = { head = animatedHead, body = animatedBody }
        , camera = nextCamera
    }


setCameraToHead : Player -> Shader.OrbitCamela -> Shader.OrbitCamela
setCameraToHead player camera =
    { camera | position = pointToPosition player.head.point }


tailOkada : Player -> Okada
tailOkada player =
    if modBy 2 (List.length player.body) == 0 then
        Oka

    else
        Da


tailNotOkada : Player -> Okada
tailNotOkada player =
    if tailOkada player == Oka then
        Da

    else
        Oka


moveAndEat : MoveTo -> Grid Cell -> Player -> ( Grid Cell, Player )
moveAndEat moveTo field player =
    let
        movedPlayer =
            move moveTo player
    in
    if validMove field player movedPlayer then
        let
            maybeCell =
                Grid.get movedPlayer.head.point field
        in
        case maybeCell of
            Just cell ->
                case cell of
                    Food _ ->
                        ( Grid.set movedPlayer.head.point Empty field
                        , { movedPlayer | body = player.head :: player.body }
                        )

                    _ ->
                        ( field, movedPlayer )

            Nothing ->
                ( field, player )

    else
        ( field, player )


validMove : Grid Cell -> Player -> Player -> Bool
validMove field before after =
    let
        min =
            0

        max =
            Grid.length field - 1

        ( x, y, z ) =
            after.head.point

        isInGrid a =
            min <= a && a <= max

        bodyPoints =
            List.map (\c -> c.point) before.body

        maybeCell =
            Grid.get after.head.point field

        tail =
            tailOkada before
    in
    isInGrid x
        && isInGrid y
        && isInGrid z
        && (List.member after.head.point bodyPoints == False)
        && (case maybeCell of
                Just cell ->
                    case cell of
                        Food okada ->
                            okada /= tail

                        _ ->
                            True

                Nothing ->
                    True
           )


move : MoveTo -> Player -> Player
move moveTo player =
    let
        head =
            player.head

        plus ( a, b, c ) ( aa, bb, cc ) =
            ( a + aa, b + bb, c + cc )

        ( dif, rotation ) =
            case moveTo of
                Xplus ->
                    ( ( 1, 0, 0 ), { radian = pi / 2, axis = vec3 0 0 -1 } )

                Xminus ->
                    ( ( -1, 0, 0 ), { radian = pi / 2, axis = vec3 0 0 1 } )

                Yplus ->
                    ( ( 0, 1, 0 ), { radian = 0, axis = vec3 0 0 -1 } )

                Yminus ->
                    ( ( 0, -1, 0 ), { radian = pi, axis = vec3 0 0 1 } )

                Zplus ->
                    ( ( 0, 0, 1 ), { radian = pi / 2, axis = vec3 1 0 0 } )

                Zminus ->
                    ( ( 0, 0, -1 ), { radian = pi / 2, axis = vec3 -1 0 0 } )

        body =
            if List.length player.body == 0 then
                []

            else
                head :: List.take (List.length player.body - 1) player.body
    in
    { player
        | head = { head | point = plus player.head.point dif, rotation = rotation }
        , body = body
    }


getClickedMoveTo : Model -> ( Float, Float ) -> Maybe MoveTo
getClickedMoveTo model pos =
    let
        origin =
            Shader.orbitCamelaPosition model.camera

        destination =
            Shader.getClickPosition model.camera (getPerspective model.size) pos

        direction =
            Vec3.direction destination origin
    in
    moveTargetBoxBlockList model.time model.field model.player
        |> List.map
            (\( moveTo, geo ) ->
                let
                    mat =
                        blockTransFormMat geo

                    triangles =
                        List.map (\( v0, v1, v2 ) -> ( Mat4.transform mat v0, Mat4.transform mat v1, Mat4.transform mat v2 )) (List.concat Asset.cube)
                in
                ( triangles, moveTo )
            )
        |> Shader.getClickedMesh origin direction
        |> Maybe.map (\moveTo -> moveTo)


blockTransFormMat : Shader.Geo -> Mat4
blockTransFormMat geo =
    Mat4.mul (Mat4.makeTranslate geo.position) (Shader.rotationToMat geo.rotation)


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

        gameOver =
            isGameOver model
    in
    { title = "岡田スネーク3D"
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
                    [ Html.span [] [ Html.text ("Lv. " ++ String.fromInt (currentLevel model)) ]
                    , Html.div
                        [ style "display" "flex"
                        , style "align-items" "center"
                        ]
                        [ Html.span
                            [ Html.Attributes.style "min-width" "40px"
                            , Html.Attributes.style "text-align" "right"
                            ]
                            [ Html.text (String.fromInt (score model)) ]
                        , Html.span
                            [ Html.Attributes.style "margin" "0 0.4rem"
                            ]
                            [ Html.text "/" ]
                        , Html.span [] [ Html.text (String.fromInt (nextExpand model)) ]
                        ]
                    ]
                , Html.div
                    [ style "width" (String.fromInt w ++ "px")
                    , style "height" (String.fromInt h ++ "px")
                    , style "position" "relative"
                    , style "border" "1px solid black"
                    , style "box-sizing" "border-box"
                    , style "overflow" "hidden"
                    , Mouse.onClick (.offsetPos >> ClickMsg)
                    , Pointer.onTouchEndWithPosition ClickMsg
                    , Wheel.onWheel Zoom
                    ]
                    (WebGL.toHtml
                        ([ width w
                         , height h
                         , style "display" "block"
                         , style "background-color" "#eee"
                         , Draggable.mouseTrigger "my-element" DragMsg
                         ]
                            ++ Draggable.touchTriggers "my-element" DragMsg
                        )
                        (fieldEntities
                            model.camera
                            perspective
                            model.meshMap.default
                            model.field
                            ++ fieldLineEntities
                                model.camera
                                perspective
                                ( model.meshMap.line, model.meshMap.hilightLine )
                                model.player.head.point
                                model.field
                            ++ playerEntities
                                model.camera
                                perspective
                                ( model.meshMap.playerHead, model.meshMap.playerBody, model.meshMap.connectorLine )
                                model.time
                                model.player
                            ++ moveTargetBoxBlockEntities
                                model.time
                                model.camera
                                perspective
                                ( model.meshMap.moveTargetBox, model.meshMap.moveTargetBoxLine )
                                model.field
                                model.player
                        )
                        :: (if gameOver then
                                [ viewGameOver ]

                            else
                                []
                           )
                    )
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
                    , Html.div []
                        [ button
                            [ Html.Events.onClick (Move Xplus)
                            ]
                            [ Html.text "X+" ]
                        , button
                            [ Html.Events.onClick (Move Xminus)
                            ]
                            [ Html.text "X-" ]
                        , button
                            [ Html.Events.onClick (Move Yplus)
                            ]
                            [ Html.text "Y+" ]
                        , button
                            [ Html.Events.onClick (Move Yminus)
                            ]
                            [ Html.text "Y-" ]
                        , button
                            [ Html.Events.onClick (Move Zplus)
                            ]
                            [ Html.text "Z+" ]
                        , button
                            [ Html.Events.onClick (Move Zminus)
                            ]
                            [ Html.text "Z-" ]
                        ]
                    ]
                , Html.div
                    [ style "text-align" "center"
                    ]
                    [ Html.a
                        [ Html.Attributes.href "https://github.com/miyanokomiya/okada-snake-3d"
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


cellSize : Float
cellSize =
    1.3


moveTargetBoxBlockEntities :
    Float
    -> Shader.OrbitCamela
    -> Mat4
    -> ( Mesh Shader.Vertex, Mesh Shader.Vertex )
    -> Grid Cell
    -> Player
    -> List WebGL.Entity
moveTargetBoxBlockEntities time camera perspective ( mesh, linesMesh ) field player =
    moveTargetBoxBlockList time field player
        |> List.map
            (\( _, geo ) ->
                let
                    uni =
                        Shader.uniforms camera
                            perspective
                            geo.position
                            (Shader.rotationToMat geo.rotation)
                in
                [ WebGL.entity Shader.vertexShader Shader.fragmentShader mesh uni
                , WebGL.entity Shader.vertexShader Shader.fragmentShader linesMesh uni
                ]
            )
        |> List.concat


moveTargetBoxBlockList : Float -> Grid Cell -> Player -> List ( MoveTo, Shader.Geo )
moveTargetBoxBlockList time field player =
    let
        diff =
            Motion.animatePositionVec time player.head.positionAnimation

        slide v =
            Vec3.add v diff
    in
    [ moveTargetBoxBlock field player ( Xplus, { position = slide (vec3 -0.5 0 0), rotation = { radian = pi / 2, axis = vec3 0 -1 0 } } )
    , moveTargetBoxBlock field player ( Xminus, { position = slide (vec3 0.5 0 0), rotation = { radian = pi / 2, axis = vec3 0 1 0 } } )
    , moveTargetBoxBlock field player ( Yplus, { position = slide (vec3 0 -0.5 0), rotation = { radian = pi / 2, axis = vec3 1 0 0 } } )
    , moveTargetBoxBlock field player ( Yminus, { position = slide (vec3 0 0.5 0), rotation = { radian = pi / 2, axis = vec3 -1 0 0 } } )
    , moveTargetBoxBlock field player ( Zplus, { position = slide (vec3 0 0 -0.5), rotation = { radian = pi, axis = vec3 0 1 0 } } )
    , moveTargetBoxBlock field player ( Zminus, { position = slide (vec3 0 0 0.5), rotation = { radian = 0, axis = vec3 0 1 0 } } )
    ]
        |> Maybe.Extra.values


moveTargetBoxBlock : Grid Cell -> Player -> ( MoveTo, Shader.Geo ) -> Maybe ( MoveTo, Shader.Geo )
moveTargetBoxBlock field player ( moveTo, geo ) =
    let
        next =
            move moveTo player

        position =
            pointToPosition next.head.point
    in
    if validMove field player next then
        Just
            ( moveTo
            , { position = Vec3.add position geo.position
              , rotation = geo.rotation
              }
            )

    else
        Nothing


pointToPosition : Grid.Point -> Vec3
pointToPosition ( x, y, z ) =
    vec3 (cellSize * toFloat x)
        (cellSize * toFloat y)
        (cellSize * toFloat z)


cellToBlock : Grid.Point -> Cell -> Maybe GeoBlock
cellToBlock point cell =
    let
        ( x, y, z ) =
            point

        position =
            pointToPosition point

        staticRotation =
            { radian = 0, axis = vec3 0 1 0 }
    in
    case cell of
        Food okada ->
            Just
                { id = String.fromInt x ++ "," ++ String.fromInt y ++ "," ++ String.fromInt z
                , okada = okada
                , geo = { position = position, rotation = staticRotation }
                , turning = staticRotation
                }

        _ ->
            Nothing


fieldLineEntities : Shader.OrbitCamela -> Mat4 -> ( Mesh Shader.Vertex, Mesh Shader.Vertex ) -> Grid.Point -> Grid Cell -> List WebGL.Entity
fieldLineEntities camera perspective ( mesh, hilightMesh ) ( hx, hy, hz ) grid =
    let
        lineSize =
            Grid.length grid

        slide =
            vec3 (cellSize / 2) (cellSize / 2) (cellSize / 2)

        repeatArray =
            Array.repeat (lineSize + 1) 0

        planeMap map =
            repeatArray
                |> Array.indexedMap
                    (\a _ ->
                        repeatArray
                            |> Array.indexedMap
                                (\b _ -> map ( a, b ))
                            |> Array.toList
                    )
                |> Array.toList
                |> List.concat

        inRange a b =
            List.member a [ b, b + 1 ]

        isHilight ( p, q ) ( s, t ) =
            inRange p s && inRange q t

        lineMesh m n =
            if isHilight m n then
                hilightMesh

            else
                mesh

        lineScale m n =
            if isHilight m n then
                vec3 12 (toFloat lineSize * cellSize) 12

            else
                vec3 2 (toFloat lineSize * cellSize) 2

        xlines =
            planeMap
                (\( a, b ) ->
                    lineEntity camera
                        perspective
                        (lineMesh ( b, a ) ( hy, hz ))
                        (lineScale ( b, a ) ( hy, hz ))
                        { position =
                            Vec3.sub
                                (pointToPosition ( 0, b, a ))
                                slide
                        , rotation = { radian = -pi / 2, axis = vec3 0 0 1 }
                        }
                )

        ylines =
            planeMap
                (\( a, b ) ->
                    lineEntity camera
                        perspective
                        (lineMesh ( b, a ) ( hx, hz ))
                        (lineScale ( b, a ) ( hx, hz ))
                        { position =
                            Vec3.sub
                                (pointToPosition ( b, 0, a ))
                                slide
                        , rotation = { radian = -pi / 2, axis = vec3 0 1 0 }
                        }
                )

        zlines =
            planeMap
                (\( a, b ) ->
                    lineEntity camera
                        perspective
                        (lineMesh ( a, b ) ( hx, hy ))
                        (lineScale ( a, b ) ( hx, hy ))
                        { position =
                            Vec3.sub
                                (pointToPosition ( a, b, 0 ))
                                slide
                        , rotation = { radian = pi / 2, axis = vec3 1 0 0 }
                        }
                )
    in
    [ xlines, ylines, zlines ] |> List.concat


fieldEntities : Shader.OrbitCamela -> Mat4 -> MeshSet -> Grid Cell -> List WebGL.Entity
fieldEntities camera perspective set grid =
    Grid.toList grid
        |> List.map
            (\( point, cell ) ->
                cellToBlock point cell
            )
        |> Maybe.Extra.values
        |> List.map
            (\block ->
                frontEntity camera perspective set 0.5 block
            )


playerCellToBlock : Mat4 -> Okada -> PlayerCell -> GeoBlock
playerCellToBlock translation okada pcell =
    let
        point =
            pcell.point

        ( x, y, z ) =
            point

        position =
            pointToPosition point
    in
    { id = String.fromInt x ++ "," ++ String.fromInt y ++ "," ++ String.fromInt z
    , okada = okada
    , geo =
        { position = Mat4.transform translation position
        , rotation = pcell.rotation
        }
    , turning = pcell.turning
    }


playerEntities : Shader.OrbitCamela -> Mat4 -> ( MeshSet, MeshSet, Mesh Shader.Vertex ) -> Float -> Player -> List WebGL.Entity
playerEntities camera perspective ( headSet, bodySet, connectorMesh ) time player =
    let
        cellEntities =
            player.head
                :: player.body
                |> List.indexedMap
                    (\i pcell ->
                        let
                            animated =
                                Motion.animatePosition time pcell.positionAnimation

                            block =
                                playerCellToBlock animated (bodyOkada (i + 1)) pcell

                            ( meshSet, scale ) =
                                if i == 0 then
                                    ( headSet, 1 )

                                else
                                    ( bodySet, 0.8 )
                        in
                        entity camera
                            perspective
                            meshSet
                            scale
                            block
                            :: (if i < List.length player.body then
                                    [ connectorEntity camera
                                        perspective
                                        connectorMesh
                                        block
                                    ]

                                else
                                    []
                               )
                    )
                |> List.concat
    in
    cellEntities


bodyOkada : Int -> Okada
bodyOkada i =
    if modBy 2 i == 1 then
        Oka

    else
        Da


connectorEntity : Shader.OrbitCamela -> Mat4 -> Mesh Shader.Vertex -> GeoBlock -> WebGL.Entity
connectorEntity camera perspective mesh block =
    let
        p =
            block.geo.position

        r =
            block.geo.rotation

        transfrom =
            Mat4.makeScale3 200 -cellSize 50
                |> Mat4.mul (Shader.rotationToMat block.turning)
                |> Mat4.mul (Shader.rotationToMat r)
    in
    WebGL.entity
        Shader.vertexShader
        Shader.fragmentShader
        mesh
        (Shader.uniforms camera perspective p transfrom)


entity : Shader.OrbitCamela -> Mat4 -> MeshSet -> Float -> GeoBlock -> WebGL.Entity
entity camera perspective set scale block =
    let
        p =
            block.geo.position

        r =
            block.geo.rotation

        transfrom =
            Mat4.makeScale3 scale scale scale
                |> Mat4.mul (Shader.rotationToMat block.turning)
                |> Mat4.mul (Shader.rotationToMat r)
    in
    WebGL.entity
        Shader.vertexShader
        Shader.fragmentShader
        (toMesh set block)
        (Shader.uniforms camera perspective p transfrom)


frontEntity : Shader.OrbitCamela -> Mat4 -> MeshSet -> Float -> GeoBlock -> WebGL.Entity
frontEntity camera perspective set scale block =
    let
        p =
            block.geo.position

        transfrom =
            Mat4.makeScale3 scale scale scale
                |> Mat4.mul (Shader.orbitCamelaRotation camera)
                |> Mat4.mul (Mat4.makeTranslate (Vec3.negate camera.position))
    in
    WebGL.entity
        Shader.vertexShader
        Shader.fragmentShader
        (toMesh set block)
        (Shader.uniforms camera perspective p transfrom)


lineEntity : Shader.OrbitCamela -> Mat4 -> Mesh Shader.Vertex -> Vec3 -> Shader.Geo -> WebGL.Entity
lineEntity camera perspective mesh size geo =
    let
        p =
            geo.position

        r =
            geo.rotation

        transform =
            Mat4.makeScale size
                |> Mat4.mul (Shader.rotationToMat r)
    in
    WebGL.entity
        Shader.vertexShader
        Shader.fragmentShader
        mesh
        (Shader.uniforms camera
            perspective
            p
            transform
        )


toMesh : MeshSet -> GeoBlock -> Mesh Shader.Vertex
toMesh set block =
    if block.okada == Oka then
        set.oka

    else
        set.da


getPerspective : ( Int, Int ) -> Mat4
getPerspective ( width, height ) =
    Mat4.makePerspective 45 (toFloat width / toFloat height) 0.01 100


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


isGameOver : Model -> Bool
isGameOver model =
    List.length (moveTargetBoxBlockList model.time model.field model.player) == 0


score : Model -> Int
score model =
    List.length model.player.body + 1


nextExpand : Model -> Int
nextExpand model =
    let
        size =
            Grid.length model.field
    in
    size * (size - 1)


currentLevel : Model -> Int
currentLevel model =
    round (toFloat (Grid.length model.field - 3) / 2)


viewGameOver : Html Msg
viewGameOver =
    Html.div
        [ style "position" "absolute"
        , style "top" "50%"
        , style "left" "50%"
        , style "transform" "translate(-50%, -50%)"
        , style "font-size" "3rem"
        , style "font-weight" "600"
        , style "color" "red"
        , style "white-space" "nowrap"
        , style "pointer-events" "none"
        ]
        [ Html.text "Game Over" ]

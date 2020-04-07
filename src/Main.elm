module Main exposing (main)

import Animation exposing (Animation)
import Array
import Block
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Draggable
import Grid exposing (Grid)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Html.Events
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Maybe.Extra
import Motion
import Random
import Random.Extra
import Round
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
    = Food Okada Shader.Rotation Shader.Rotation
    | Empty


type alias PlayerCell =
    { point : Grid.Point
    , rotation : Shader.Rotation
    , turning : Shader.Rotation
    }


initField : Grid Cell
initField =
    Grid.initialize 4
        (\( x, y, z ) ->
            if modBy 5 (x + y + z) == 0 then
                Food Oka { radian = 0, axis = vec3 0 1 0 } { radian = 0, axis = vec3 0 1 0 }

            else if modBy 5 (x + y + z) == 2 then
                Food Da { radian = 0, axis = vec3 0 1 0 } { radian = 0, axis = vec3 0 1 0 }

            else
                Empty
        )


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
    10 + toFloat (4 * level)


initModel : Int -> Model
initModel level =
    { time = 0
    , field = initField
    , player =
        { head = { point = ( 0, 2, 0 ), rotation = { radian = 0, axis = vec3 0 1 0 }, turning = { radian = 0, axis = vec3 0 1 0 } }
        , body =
            [ { point = ( 0, 1, 0 ), rotation = { radian = 0, axis = vec3 0 1 0 }, turning = { radian = 0, axis = vec3 0 1 0 } }
            , { point = ( 0, 0, 0 ), rotation = { radian = 0, axis = vec3 0 1 0 }, turning = { radian = 0, axis = vec3 0 1 0 } }
            ]
        }
    , camera = ( cameraRadius level, pi / 8, pi / 16 )
    , size = ( 400, 600 )
    , downTime = 0
    , drag = Draggable.init
    , meshMap =
        { default = { oka = okadaMesh defaultColor Oka, da = okadaMesh defaultColor Da }
        , playerHead = { oka = okadaMesh playerHeadColor Oka, da = okadaMesh playerHeadColor Da }
        , playerBody = { oka = okadaMesh playerBodyColor Oka, da = okadaMesh playerBodyColor Da }
        , line = Block.lineLoopMesh (vec3 180 180 180) [ vec3 0 0 0, vec3 0 1 0 ]
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
            , Cmd.none
            )

        Delta dt ->
            let
                time =
                    model.time + dt
            in
            ( { model
                | time = time
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
                | camera = ( cr, ca - dx / 30, limitRadian (cb - dy / 30) )
                , downTime = model.downTime + 1
              }
            , Cmd.none
            )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model

        Move moveTo ->
            let
                ( nextFiled, nextPlayer ) =
                    moveAndEat moveTo model.field model.player
            in
            ( { model
                | field = nextFiled
                , player = nextPlayer
              }
            , Cmd.none
            )


tailOkada : Player -> Okada
tailOkada player =
    if modBy 2 (List.length player.body) == 0 then
        Oka

    else
        Da


moveAndEat : MoveTo -> Grid Cell -> Player -> ( Grid Cell, Player )
moveAndEat moveTo field player =
    let
        movedPlayer =
            move moveTo player
    in
    if validMove field movedPlayer then
        let
            maybeCell =
                Grid.get movedPlayer.head.point field
        in
        case maybeCell of
            Just cell ->
                case cell of
                    Food _ _ _ ->
                        ( Grid.set movedPlayer.head.point Empty field
                        , { movedPlayer | body = player.head :: player.body }
                        )

                    _ ->
                        ( field, movedPlayer )

            Nothing ->
                ( field, player )

    else
        ( field, player )


validMove : Grid Cell -> Player -> Bool
validMove field player =
    let
        min =
            0

        max =
            Grid.length field - 1

        ( x, y, z ) =
            player.head.point

        isInGrid a =
            min <= a && a <= max

        bodyPoints =
            List.map (\c -> c.point) player.body

        maybeCell =
            Grid.get player.head.point field

        tail =
            tailOkada player
    in
    isInGrid x
        && isInGrid y
        && isInGrid z
        && (List.member player.head.point bodyPoints == False)
        && (case maybeCell of
                Just cell ->
                    case cell of
                        Food okada _ _ ->
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
            head :: List.take (List.length player.body - 1) player.body
    in
    { player
        | head = { head | point = plus player.head.point dif, rotation = rotation }
        , body = body
    }


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
                    [ Html.span [] [ Html.text (timeText model.time) ]
                    ]
                , Html.div
                    [ width w
                    , height h
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
                        (fieldEntities
                            model.camera
                            perspective
                            model.meshMap.default
                            model.field
                            ++ fieldLineEntities
                                model.camera
                                perspective
                                model.meshMap.line
                                model.field
                            ++ playerEntities
                                model.camera
                                perspective
                                ( model.meshMap.playerHead, model.meshMap.playerBody )
                                (Array.length model.field)
                                model.player
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
                        [ Html.span
                            [ Html.Attributes.style "min-width" "40px"
                            , Html.Attributes.style "text-align" "right"
                            ]
                            [ Html.text "??" ]
                        , Html.span
                            [ Html.Attributes.style "margin" "0 0.4rem"
                            ]
                            [ Html.text "/" ]
                        , Html.span [] [ Html.text "??" ]
                        ]
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


timeText : Float -> String
timeText time =
    Round.round 1 (time / 1000)


cellSize : Float
cellSize =
    1.3


pointToPosition : Int -> Grid.Point -> Vec3
pointToPosition lineSize ( x, y, z ) =
    let
        slide =
            (toFloat lineSize - 1) / 2
    in
    vec3 (cellSize * (toFloat x - slide))
        (cellSize * (toFloat y - slide))
        (cellSize * (toFloat z - slide))


cellToBlock : Int -> Grid.Point -> Cell -> Maybe GeoBlock
cellToBlock lineSize point cell =
    let
        ( x, y, z ) =
            point

        position =
            pointToPosition lineSize point
    in
    case cell of
        Food okada rotation turning ->
            Just
                { id = String.fromInt x ++ "," ++ String.fromInt y ++ "," ++ String.fromInt z
                , okada = okada
                , geo = { position = position, rotation = rotation }
                , turning = turning
                }

        _ ->
            Nothing


fieldLineEntities : Shader.OrbitCamela -> Mat4 -> Mesh Shader.Vertex -> Grid Cell -> List WebGL.Entity
fieldLineEntities camera perspective mesh grid =
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

        xlines =
            planeMap
                (\( a, b ) ->
                    lineEntity camera
                        perspective
                        mesh
                        (toFloat lineSize * cellSize)
                        { position =
                            Vec3.sub
                                (pointToPosition lineSize ( 0, b, a ))
                                slide
                        , rotation = { radian = -pi / 2, axis = vec3 0 0 1 }
                        }
                )

        ylines =
            planeMap
                (\( a, b ) ->
                    lineEntity camera
                        perspective
                        mesh
                        (toFloat lineSize * cellSize)
                        { position =
                            Vec3.sub
                                (pointToPosition lineSize ( b, 0, a ))
                                slide
                        , rotation = { radian = -pi / 2, axis = vec3 0 1 0 }
                        }
                )

        zlines =
            planeMap
                (\( a, b ) ->
                    lineEntity camera
                        perspective
                        mesh
                        (toFloat lineSize * cellSize)
                        { position =
                            Vec3.sub
                                (pointToPosition lineSize ( a, b, 0 ))
                                slide
                        , rotation = { radian = pi / 2, axis = vec3 1 0 0 }
                        }
                )
    in
    [ xlines, ylines, zlines ] |> List.concat


fieldEntities : Shader.OrbitCamela -> Mat4 -> MeshSet -> Grid Cell -> List WebGL.Entity
fieldEntities camera perspective set grid =
    let
        lineSize =
            Grid.length grid
    in
    Grid.toList grid
        |> List.map
            (\( point, cell ) ->
                cellToBlock lineSize point cell
            )
        |> Maybe.Extra.values
        |> List.map
            (\block ->
                frontEntity camera perspective set 0.5 block
            )


playerCellToBlock : Int -> Okada -> PlayerCell -> GeoBlock
playerCellToBlock fieldSize okada pcell =
    let
        point =
            pcell.point

        ( x, y, z ) =
            point

        position =
            pointToPosition fieldSize point
    in
    { id = String.fromInt x ++ "," ++ String.fromInt y ++ "," ++ String.fromInt z
    , okada = okada
    , geo = { position = position, rotation = pcell.rotation }
    , turning = pcell.turning
    }


playerEntities : Shader.OrbitCamela -> Mat4 -> ( MeshSet, MeshSet ) -> Int -> Player -> List WebGL.Entity
playerEntities camera perspective ( headSet, bodySet ) fieldSize player =
    entity camera perspective headSet 1 (playerCellToBlock fieldSize Oka player.head)
        :: (player.body
                |> List.indexedMap
                    (\i pcell ->
                        entity camera
                            perspective
                            bodySet
                            0.8
                            (playerCellToBlock fieldSize
                                (if modBy 2 i == 1 then
                                    Oka

                                 else
                                    Da
                                )
                                pcell
                            )
                    )
           )


entity : Shader.OrbitCamela -> Mat4 -> MeshSet -> Float -> GeoBlock -> WebGL.Entity
entity camera perspective set scale block =
    let
        p =
            block.geo.position

        r =
            block.geo.rotation

        transfrom =
            Mat4.mul (Mat4.mul (Shader.rotationToMat r) (Shader.rotationToMat block.turning)) (Mat4.makeScale3 scale scale scale)
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
            Mat4.mul (Shader.orbitCamelaRotation camera) (Mat4.makeScale3 scale scale scale)
    in
    WebGL.entity
        Shader.vertexShader
        Shader.fragmentShader
        (toMesh set block)
        (Shader.uniforms camera perspective p transfrom)


lineEntity : Shader.OrbitCamela -> Mat4 -> Mesh Shader.Vertex -> Float -> Shader.Geo -> WebGL.Entity
lineEntity camera perspective mesh size geo =
    let
        p =
            geo.position

        r =
            geo.rotation
    in
    WebGL.entity
        Shader.vertexShader
        Shader.fragmentShader
        mesh
        (Shader.uniforms camera
            perspective
            p
            (Mat4.mul (Shader.rotationToMat r) (Mat4.makeScale3 size size size))
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

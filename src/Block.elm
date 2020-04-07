module Block exposing
    ( lineLoopMesh
    , mesh
    , meshCube
    , meshCubeLine
    , meshDa
    , meshOka
    )

import Asset
import Math.Vector3 as Vec3 exposing (Vec3)
import Shader
import WebGL exposing (Mesh)


lineLoopMesh : Vec3 -> List Vec3 -> Mesh Shader.Vertex
lineLoopMesh color list =
    list
        |> List.map (\v -> Shader.Vertex (Vec3.scale (1 / 255) color) v)
        |> WebGL.lineLoop


linesMesh : Vec3 -> List ( Vec3, Vec3 ) -> Mesh Shader.Vertex
linesMesh color list =
    list
        |> List.map
            (\( a, b ) ->
                ( Shader.Vertex (Vec3.scale (1 / 255) color) a
                , Shader.Vertex (Vec3.scale (1 / 255) color) b
                )
            )
        |> WebGL.lines


mesh : List ( Shader.Triangle, Vec3 ) -> Mesh Shader.Vertex
mesh list =
    list
        |> List.map (\( tri, color ) -> face color tri)
        |> List.concat
        |> WebGL.triangles


meshCube : Mesh Shader.Vertex
meshCube =
    (Asset.cube
        |> List.concat
        |> List.map (\tri -> ( tri, Vec3.vec3 0 0 255 ))
    )
        |> mesh


meshCubeLine : Mesh Shader.Vertex
meshCubeLine =
    linesMesh (Vec3.vec3 0 0 0) Asset.cubeLine


meshOka : Vec3 -> Vec3 -> Mesh Shader.Vertex
meshOka faceColor sideColor =
    List.append
        (Asset.okaFace
            |> List.concat
            |> List.map (\tri -> ( tri, faceColor ))
        )
        (Asset.okaSide
            |> List.concat
            |> List.map (\tri -> ( tri, sideColor ))
        )
        |> mesh


meshDa : Vec3 -> Vec3 -> Mesh Shader.Vertex
meshDa faceColor sideColor =
    List.append
        (Asset.daFace
            |> List.concat
            |> List.map (\tri -> ( tri, faceColor ))
        )
        (Asset.daSide
            |> List.concat
            |> List.map (\tri -> ( tri, sideColor ))
        )
        |> mesh


face : Vec3 -> Shader.Triangle -> List ( Shader.Vertex, Shader.Vertex, Shader.Vertex )
face color ( a, b, c ) =
    let
        vertex position =
            Shader.Vertex (Vec3.scale (1 / 255) color) position
    in
    [ ( vertex a, vertex b, vertex c )
    ]

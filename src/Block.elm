module Block exposing (mesh, meshDa, meshOka)

import Asset
import Math.Vector3 as Vec3 exposing (Vec3)
import Shader
import WebGL exposing (Mesh)


mesh : List ( Shader.Triangle, Vec3 ) -> Mesh Shader.Vertex
mesh list =
    list
        |> List.map (\( tri, color ) -> face color tri)
        |> List.concat
        |> WebGL.triangles


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

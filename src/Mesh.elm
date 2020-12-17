module Mesh exposing (Mesh, Edge, testMesh, vertices, edgeCoords)

import Array exposing (Array)
import List.Nonempty
import Math.Vector2 exposing (..)

type alias Mesh =
        { points : Array Vec2
        , shapes : List Edge
        }

type Edge
        = ChainedEdge Int Edge
        | LastEdge Int

edgeCoords : Edge -> Mesh -> List.Nonempty.Nonempty ( ( Float, Float ), ( Float, Float ) )
edgeCoords edge mesh =
        vertices edge mesh
                |> loopedPairs

-- Take a list [a, b, c, ..., z] and return a list of pairs [(a, b), (b, c), ... (z, a)]
loopedPairs : List.Nonempty.Nonempty a -> List.Nonempty.Nonempty (a, a)
loopedPairs list =
        loopedPairsImpl
                ( List.Nonempty.head list )
                list

loopedPairsImpl : a -> List.Nonempty.Nonempty a -> List.Nonempty.Nonempty (a, a)
loopedPairsImpl first ( List.Nonempty.Nonempty head tail ) =
        case tail of
                mid :: rest ->
                        List.Nonempty.Nonempty
                                ( head, mid )
                                ( List.Nonempty.toList
                                        ( loopedPairsImpl
                                                first
                                                ( List.Nonempty.Nonempty mid rest )
                                        )
                                )

                [] ->
                        List.Nonempty.Nonempty
                                ( head, first )
                                []

vertices : Edge -> Mesh -> List.Nonempty.Nonempty ( Float, Float )
vertices edge mesh =
        case edge of
                ChainedEdge pointId next ->
                        List.Nonempty.Nonempty
                                ( coords pointId mesh.points )
                                ( vertices next mesh
                                        |> List.Nonempty.toList
                                )

                LastEdge pointId ->
                        List.Nonempty.Nonempty
                                ( coords pointId mesh.points )
                                []

-- TODO: makes more sense to return Vec2 instead?
coords : Int -> Array Vec2 -> ( Float, Float )
coords pointId points =
        let
            -- TODO: find a way to do this without withDefault
            point = Maybe.withDefault
                        ( vec2 0.0 0.0 )
                        ( Array.get pointId points )
        in
            ( getX point
            , getY point
            )

testMesh =
        { points =
                Array.fromList
                        [ vec2 10.0 20.0
                        , vec2 20.0 30.0
                        , vec2 0.0 20.5
                        ]
        , shapes =
                [ ChainedEdge 1 ( ChainedEdge 2 ( LastEdge 3 ) ) ]
        }

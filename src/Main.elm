module Main exposing (main)

import Playground exposing (..)
import Math.Vector2 exposing (..)
import Array exposing (Array)

main =
        picture ( draw mesh )

type alias Mesh =
        { points : Array Vec2
        , shapes : List Edge
        }

type Edge
        = ChainedEdge Int Edge
        | LastEdge Int

draw : Mesh -> List Shape
draw m =
        ( List.map
                ( \edge -> ( drawFace m.points edge ) )
                m.shapes
        ) |> List.concat

drawFace : Array Vec2 -> Edge -> List Shape
drawFace points edge =
        List.map
                (\(x, y) -> move x y ( circle black 2 ) )
                ( vertices points edge )


vertices : Array Vec2 -> Edge -> List ( Number, Number )
vertices points edge =
        case edge of
                ChainedEdge pointId next ->
                        ( coords pointId points ) :: ( vertices points next )

                LastEdge pointId ->
                        coords pointId points :: []

coords : Int -> Array Vec2 -> ( Number, Number )
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

mesh =
        { points =
                Array.fromList
                        [ vec2 10.0 20.0
                        , vec2 20.0 30.0
                        , vec2 0.0 20.5
                        ]
        , shapes =
                [ ChainedEdge 1 ( ChainedEdge 2 ( LastEdge 3 ) ) ]
        }

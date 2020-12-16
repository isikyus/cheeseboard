module Main exposing (main)

import Array exposing (Array)
import List.Nonempty
import Math.Vector2 exposing (..)
import Playground exposing (..)

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
        ( List.Nonempty.append
                ( vertices points edge
                  |> loopedPairs
                  |> List.Nonempty.map line
                )
                ( debugDots points edge )
        )
        |> List.Nonempty.toList

debugDots points edge =
        ( vertices points edge
          |> List.Nonempty.map
                ( \(x, y) ->
                        circle blue 2
                        |> move x y
                )
        )


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

line : ( ( Number, Number), ( Number, Number ) ) -> Shape
line ( ( x1, y1 ), ( x2 , y2 ) ) =
        let
            length = Math.Vector2.distance
                        ( vec2 x1 y1 )
                        ( vec2 x2 y2 )

            -- Playground.rotate rotates about the _centre_ of the shape, so I need to move the line
            -- to the centre of the interval gap it goes in, rather than one end as you might expect.
            lineCentreX = x1 + ( ( x2 - x1 ) / 2 )
            lineCentreY = y1 + ( ( y2 - y1 ) / 2 )
        in
                ( rectangle black 1 length )
                |> move lineCentreX lineCentreY
                |> rotate
                        ( 90
                        + radiansToDegrees
                                ( atan2 ( y2 - y1 ) ( x2 - x1 ) )
                        )

radiansToDegrees : Number -> Number
radiansToDegrees theta =
        -- Elm angle functions return radians, so ( degrees 1 ) is one degree's worth of radians,
        -- and 1 radian ( radians 1 ) / ( degrees 1 ) is therefore one radian in degrees.
        theta * ( radians 1 ) / ( degrees 1 )

vertices : Array Vec2 -> Edge -> List.Nonempty.Nonempty ( Number, Number )
vertices points edge =
        case edge of
                ChainedEdge pointId next ->
                        List.Nonempty.Nonempty
                                ( coords pointId points )
                                ( vertices points next |> List.Nonempty.toList )

                LastEdge pointId ->
                        List.Nonempty.Nonempty
                                ( coords pointId points )
                                []

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

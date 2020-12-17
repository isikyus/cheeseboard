module Main exposing (main)

import List.Nonempty
import Math.Vector2 exposing (..)
import Playground exposing (..)

import Mesh

main =
        picture
                ( draw ( Mesh.rectangle 100 200 ) )

draw : Mesh.Mesh -> List Shape
draw m =
        ( List.map
                ( \edge -> drawFace edge m )
                m.shapes
        )
        |> List.concat

drawFace : Mesh.Edge -> Mesh.Mesh -> List Shape
drawFace edge mesh =
        ( List.Nonempty.append
                ( List.Nonempty.map
                        line
                        ( Mesh.edgeCoords edge mesh )
                )
                ( debugDots ( Mesh.vertices edge mesh ) )
        )
        |> List.Nonempty.toList

debugDots vertices =
        List.Nonempty.map
                ( \(x, y) ->
                        circle blue 2
                        |> move x y
                )
                vertices


line : ( ( Float, Float), ( Float, Float ) ) -> Shape
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

radiansToDegrees : Float -> Float
radiansToDegrees theta =
        -- Elm angle functions return radians, so ( degrees 1 ) is one degree's worth of radians,
        -- and 1 radian ( radians 1 ) / ( degrees 1 ) is therefore one radian in degrees.
        theta * ( radians 1 ) / ( degrees 1 )

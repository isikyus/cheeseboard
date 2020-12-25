module Geometry exposing (intersectLineWithSegment)

import Math.Vector2 exposing (..)

type Line = LineFromPoints Vec2 Vec2

-- TODO: enforce that the line and segment each have nonzero length.
intersectLineWithSegment : ( Vec2, Vec2 ) -> ( Vec2, Vec2 ) -> Maybe Vec2
intersectLineWithSegment ( linePointA, linePointB ) ( segmentStart, segmentEnd ) =
        let
                alongLine = sub linePointB linePointA
                lineCoefficient = ccw90Degrees alongLine
                alongSegment = sub segmentEnd segmentStart
                betweenStarts = sub linePointA segmentStart
                positionInSegment = ( dot lineCoefficient betweenStarts ) / ( dot lineCoefficient alongSegment )
        in
          -- TODO: should use a division that returns Nothing on division by zero.
          Just ( add segmentStart ( scale positionInSegment alongSegment ) )
                

ccw90Degrees : Vec2 -> Vec2
ccw90Degrees v =
        toRecord v
        |> ( \r -> { x = -r.y, y = r.x } )
        |> fromRecord

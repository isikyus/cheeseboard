module Geometry exposing (intersectLineWithSegment, lineFromPoints, translateLine, segmentBetween, extendSegment)

import Math.Vector2 exposing (..)

-- TODO: only exposing so tests can offset a line and convert to segment
type Line = LineFromPoints Vec2 Vec2

lineFromPoints : Vec2 -> Vec2 -> Line
lineFromPoints a b =
  LineFromPoints a b

-- Translate a line by the given vector, producing a new parallel (or coincident) line
translateLine : Vec2 -> Line -> Line
translateLine offset ( LineFromPoints a b ) =
  lineFromPoints
    ( add offset a )
    ( add offset b )

type Segment = SegmentBetween Vec2 Vec2

segmentBetween : Vec2 -> Vec2 -> Segment
segmentBetween a b =
  SegmentBetween a b

extendSegment : Segment -> Line
extendSegment ( SegmentBetween a b ) =
  lineFromPoints a b

-- Like normal division, but return Nothing rather than dividing by zero.
maybeQuotient : Float -> Float -> Maybe Float
maybeQuotient dividend divisor =
  if
    divisor == 0.0
  then
    Nothing
  else
    Just ( dividend / divisor )

-- TODO: enforce that the line and segment each have nonzero length.
intersectLineWithSegment : Line -> Segment -> Maybe Vec2
intersectLineWithSegment ( LineFromPoints linePointA linePointB ) ( SegmentBetween segmentStart segmentEnd ) =
        let
                alongLine = sub linePointB linePointA
                lineCoefficient = ccw90Degrees alongLine
                alongSegment = sub segmentEnd segmentStart
                betweenStarts = sub linePointA segmentStart
                positionInSegment =
                  maybeQuotient
                    ( dot lineCoefficient betweenStarts )
                    ( dot lineCoefficient alongSegment )
        in
          Maybe.map
            -- TODO: only return points actually on the segment
            ( \pos -> add segmentStart ( scale pos alongSegment ) )
            positionInSegment

ccw90Degrees : Vec2 -> Vec2
ccw90Degrees v =
        toRecord v
        |> ( \r -> { x = -r.y, y = r.x } )
        |> fromRecord

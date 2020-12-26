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

segmentStart : Segment -> Vec2
segmentStart ( SegmentBetween start end ) =
  start

segmentEnd : Segment -> Vec2
segmentEnd ( SegmentBetween start end ) =
  end

-- A vector "along" the given segment, from start to end.
alongSegment : Segment -> Vec2
alongSegment ( SegmentBetween start end ) =
  sub end start

extendSegment : Segment -> Line
extendSegment ( SegmentBetween start end ) =
  lineFromPoints start end

-- Finds the point a given distance along the segment, where the segment start is 0.0 and end is 1.0
-- Not defined for points outside that range.
pointInSegment : Float -> Segment -> Maybe Vec2
pointInSegment distance segment =
  if
    0.0 <= distance && distance <= 1.0
  then
    Just
      ( add
        ( segmentStart segment )
        ( scale distance ( alongSegment segment ) )
      )
  else
    Nothing

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
intersectLineWithSegment ( LineFromPoints linePointA linePointB ) segment =
        let
                alongLine = sub linePointB linePointA
                lineCoefficient = ccw90Degrees alongLine
                betweenStarts = sub linePointA ( segmentStart segment )
        in
            maybeQuotient
              ( dot lineCoefficient betweenStarts )
              ( dot lineCoefficient ( alongSegment segment ) )
            |> Maybe.andThen
              ( \pos -> pointInSegment pos segment )

ccw90Degrees : Vec2 -> Vec2
ccw90Degrees v =
        toRecord v
        |> ( \r -> { x = -r.y, y = r.x } )
        |> fromRecord

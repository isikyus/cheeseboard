module GeometryExample exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Math.Vector2 exposing (..)
import Test exposing (..)

import Geometry exposing (..)

fuzzPoint : Fuzzer Vec2
fuzzPoint =
  Fuzz.map2
    vec2
    nonUnitRatio
    nonUnitRatio

-- A positive or negative float value, but never exactly +-1 or 0, nor degenerate (NaN, Infinity)
-- Thus multiplying by a nonUnitRatio float is guaranteed to give a different, non-degenerate floating-point value.
nonUnitRatio : Fuzzer Float
nonUnitRatio =
  Fuzz.oneOf
    [ ( Fuzz.floatRange -1000000000 -1.1 )
    , ( Fuzz.floatRange -0.9 0.000000001 )
    , ( Fuzz.floatRange 0.000000001 0.9 )
    , ( Fuzz.floatRange 1.1 10000000000 )
    ]

-- Given a point, fuzz another point that doesn't coincide with it.
offsetPoint : Vec2 -> Fuzzer Vec2
offsetPoint initial =
  Fuzz.map2
    ( \rX -> \rY ->
      vec2
        ( ( getX initial ) * rX )
        ( ( getY initial ) * rY )
    )
    nonUnitRatio
    nonUnitRatio

-- Fuzz a line passing through the given point.
-- TODO: only generates lines starting at that point at the moment
lineThrough : ( Vec2 -> Vec2 -> a ) -> Vec2 -> Fuzzer a
lineThrough lineConstructor point =
  Fuzz.map
    ( \otherPoint -> lineConstructor point otherPoint )
    ( offsetPoint point )

vec2Within : Expect.FloatingPointTolerance -> Vec2 -> Vec2 -> Expect.Expectation
vec2Within tolerance expected =
  Expect.all
    [ getX
      >> Expect.within
        tolerance
        ( getX expected )
    , getY
      >> Expect.within
        tolerance
        ( getY expected )
    ]

notNothing : ( a -> Expect.Expectation ) -> Maybe a -> Expect.Expectation
notNothing expectation maybe =
  case maybe of
    Just a ->
      expectation a

    Nothing ->
      Expect.fail "Expected Just <something> but got Nothing"

-- The actual Elm geometry package seems to think this is a reasonable value.
defaultTolerance : Expect.FloatingPointTolerance
defaultTolerance =
  Expect.AbsoluteOrRelative 1e-12 1e-12

-- TODO: write better geometry primitives then come back and make this cleaner and more general
suite : Test
suite =
  describe "intersectLineWithSegment"
    [ ( let
          point = vec2 10 -3.2
        in
          fuzz2
            ( lineThrough lineFromPoints point )
            ( lineThrough segmentBetween point )
            "returns the intersection point"
            ( \line -> \segment ->
                intersectLineWithSegment line segment
                |> notNothing ( vec2Within defaultTolerance point )
            )
      )
    , ( let
          offset = vec2 -17.88 0.03
        in
          fuzz
            ( lineThrough segmentBetween ( vec2 10 -3.2 ) )
            "finds no intersection between parallel lines"
            ( \segment ->
                intersectLineWithSegment
                  ( translateLine
                    offset
                    ( extendSegment segment )
                  )
                  segment
                |> Expect.equal Nothing
            )
      )
    , ( fuzz ( lineThrough segmentBetween ( vec2 10 -3.2 ) )
          "finds no intersection between coincident lines"
          ( \segment ->
              intersectLineWithSegment
                ( extendSegment segment )
                segment
              |> Expect.equal Nothing
          )
      )
    , ( fuzz2
          ( lineThrough segmentBetween ( vec2 10 -3.2 ) )
          ( fuzzPoint )
          "finds no intersection between any two lines with the same slope"
          ( \segment -> \offset ->
              intersectLineWithSegment
                ( translateLine
                  offset
                  ( extendSegment segment )
                )
                segment
              |> Expect.equal Nothing
          )
      )
    ]

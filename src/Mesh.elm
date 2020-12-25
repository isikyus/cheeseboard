module Mesh exposing (Mesh, Edge, rectangle, vertices, edgeCoords)

import Array exposing (Array)
import Geometry exposing (intersectLineWithSegment)
import List.Nonempty
import Math.Vector2 exposing (..)

type alias Mesh =
        { points : Array Vec2
        , shapes : List Edge
        }

type Edge
        = ChainedEdge PointRef Edge
        | LastEdge PointRef

rectangle : Float -> Float -> Mesh
rectangle width height =
        { points =
                Array.fromList
                        [ vec2 0.0 0.0
                        , vec2 width 0.0
                        , vec2 width height
                        , vec2 0.0 height
                        ]
        , shapes =
                [ ChainedEdge 1 ( ChainedEdge 2 ( ChainedEdge 3 ( LastEdge 4 ) ) )
                ]
        }

type alias PointRef = Int

type PointUpdate
        = NewPoint PointRef Vec2
        | ExistingPoint PointRef

-- Split a face of a mesh on a line defined by the two given points,
-- and return a new mesh with the original face replaced by the two
-- halves. Assumes the original face is convex.
-- Does nothing if the line does not cross the given face.
--split : Vec2 -> Vec2 -> Edge -> Mesh -> Mesh
--split a b face mesh =
--        let
--            newPoints, newFaces = breakFaces a b ( loopedPairs face mesh ) mesh
--            newPoints = 
  --                      ( List.Nonempty.concat newFaces
    --                            |> List.Nonempty.toList
      --                          |> filterMap
        --                                ( \vertex ->
          --                                      case vertex of
            --                                            NewPoint ref vec2 ->
              --                                                  Just vertex
--
  --                                                      ExistingPoint _ ->
    --                                                            Nothing
      --                                  )
        --                )
--        in
--                Mesh
--                        newPoints
--                        ( List.Nonempty.concat
--                                ( List.Nonempty.map
--                                        pointsToEdges
--                                        newFaces
--                                )
--                                mesh.faces
--                        )

type PointWithId = WithId PointRef Vec2

split : Vec2 -> Vec2 -> ( List.Nonempty.Nonempty ( PointWithId, PointWithId ) -> Mesh -> ( Array Vec2, List.Nonempty.Nonempty ( List.Nonempty.Nonempty PointUpdate ) ) )
split a b face mesh =
        case face of
                List.Nonempty.Nonempty last [] ->
                        case ( intersectLineWithSegment (a, b) last ) of
                                Just intersection ->
                                        let
                                            newPoints = Array.push intersection mesh.points
                                        in
                                           ( newPoints
                                           -- TODO: actually do second face
                                           , [ LastEdge ( ( Array.length newPoints ) - 1 ) ] 
                                           )

                                Nothing ->
                                        let
                                            ( WithId ref _ ) = last
                                        in
                                                ( mesh.points
                                                -- TODO: actually do second face
                                                , [ LastEdge ref ]
                                                )

 
                List.Nonempty.Nonempty head ( mid :: tail ) ->
                        let
                            -- TODO: handle remaining faces
                            ( childPoints, ( childEdge :: _ ) ) = split a b ( mid :: tail ) mesh
                        in
                                case ( intersectLineWithSegment (a, b) head ) of
                                        Just intersection ->
                                                let
                                                    newPoints = Array.push intersection childPoints
                                                in
                                                   ( newPoints
                                                   , ChainedEdge
                                                        ( ( Array.length newPoints ) - 1 ) 
                                                        childEdge
                                                   )

                                        Nothing ->
                                                let
                                                    ( WithId ref _ ) = head
                                                in
                                                        ( mesh.points
                                                        , ChainedEdge ref childEdge
                                                        )

--pointsToEdges : List.Nonempty.Nonempty PointUpdate -> Edge
--pointsToEdges List.Nonempty.Nonempty head tail =
--        case tail of
--                mid :: rest ->
--                        ChainedEdge
--                              ( newPointRef head )
--                              ( pointsToEdges
--                                        ( List.Nonempty.Nonempty mid rest )
--                              )
--                []
--                        LastEdge ( newPointRef head )

newPointRef : PointUpdate -> PointRef
newPointRef edge =
        case edge of
                NewPoint id _ ->
                        id

                ExistingPoint id ->
                        id


--        let
--                intersections = 
--                        filterMap
--                                intersectLineWithSegment
--                                ( edgeCoords face mesh
--                                        |> List.Nonempty.toList 
--                                )
--        in
--            case intersections of
--                    [] ->
--                            mesh
--
--                    a :: b :: [] ->
--

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

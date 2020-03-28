{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Shape where

import Linear.V3 (V3 (..))
import qualified Linear.V3 as V3 (cross)
import qualified Vector as V
import Vector ((.*.), (***), (+*+), (-*-))

data Shape =
  -- Vertices are in "clockwise" order, when faced from the front side
  Triangle {a :: V3 Double, b :: V3 Double, c :: V3 Double}
  | Sphere {
    center :: V3 Double,
    radius :: Double
  } deriving (Eq, Show)

-- given a shape, origin point, and direction, return the intersection point
intersect :: Shape -> V.Ray -> Maybe (V3 Double)
intersect (Triangle v1 v2 v3) (V.Ray origin ray) =
  -- https://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm
  -- I have no idea how this works though :(
  let edge1 = v2 -*- v1
      edge2 = v3 -*- v1
      h = V3.cross ray edge2
      a = edge1 .*. h
  in if -0.000001 < a && a < 0.000001 then Nothing
      else
        let f = 1 / a
            s = origin -*- v1
            u = f * (s .*. h)
        in if u < 0 || u > 1 then Nothing
          else
            let
              q = V3.cross s edge1
              v = f * (ray .*. q)
            in if v < 0 || u + v > 1 then Nothing
                else
                  let t = f * (edge2 .*. q)
                  in if t > 0.000001 then Just $ origin +*+ (t *** ray)
                    else Nothing
intersect (Sphere center radius) (V.Ray origin ray) =
  let rayDirection = V.normalize ray
      relCenter = center -*- origin
      rayProjection = relCenter .*. rayDirection
  in if rayProjection < 0 then Nothing
     else let closestApproachSquare =  V.normSquare relCenter - rayProjection * rayProjection
              radiusSquare = radius * radius
          in if closestApproachSquare > radiusSquare then Nothing
             else let dist = sqrt (radiusSquare - closestApproachSquare)
                  in Just $ origin +*+ ((rayProjection - dist) *** rayDirection)

normal :: Shape -> V3 Double -> V3 Double
-- Triangle normal doesn't depend on where the intersection point is located
normal (Triangle a b c) _ = V.normalize $ V3.cross (c -*- a) (b -*- a)
normal (Sphere center _) position = V.normalize $ position -*- center

intersectData :: V.Ray -> Shape -> Maybe (V3 Double, V3 Double)
intersectData ray shape =
  let intersection = intersect shape ray
  in case intersection of
      Nothing -> Nothing
      Just point -> Just (point, normal shape point)

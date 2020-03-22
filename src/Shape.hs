module Shape
    ( Shape,
      intersect,
      normal,
      Triangle (..),
      Ellipsoid (..)
    ) where

import Linear.V3 (V3 (..))
import qualified Linear.V3 as V3 (cross)
import qualified Vector as V
import Vector ((.*.), (***), (+*+), (-*-))

class Shape a where
  -- given a shape, origin point, and direction, return the intersection point
  intersect :: a -> V3 Double -> V3 Double -> Maybe (V3 Double)
  normal :: a -> V3 Double -> V3 Double

-- Vertices are in "clockwise" order, when faced from the front side
data Triangle = Triangle {a :: V3 Double, b :: V3 Double, c :: V3 Double}
instance Shape Triangle where
  -- https://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm
  -- I have no idea how this works though :(
  intersect (Triangle v1 v2 v3) origin ray =
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
  -- Triangle normal doesn't depend on where the intersection point is located
  normal (Triangle a b c) _ = V.normalize $ V3.cross (c -*- a) (b -*- a)

data Ellipsoid = Ellipsoid {
  center :: V3 Double,
  r1 :: V3 Double,
  r2 :: V3 Double,
  r3 :: V3 Double
}

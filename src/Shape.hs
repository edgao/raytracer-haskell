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
  intersect :: a -> V3 Double -> Maybe (V3 Double)
  normal :: a -> V3 Double -> V3 Double

-- Vertices are in "clockwise" order, when faced from the front side
data Triangle = Triangle {a :: V3 Double, b :: V3 Double, c :: V3 Double}
instance Shape Triangle where
  intersect (Triangle a b c) (V3 x y z) = Nothing
  -- Triangle normal doesn't depend on where the intersection point is located
  normal (Triangle a b c) _ = V.normalize $ V3.cross (c -*- a) (b -*- a)

data Ellipsoid = Ellipsoid {center :: V3 Double, r1 :: V3 Double, r2 :: V3 Double, r3 :: V3 Double}

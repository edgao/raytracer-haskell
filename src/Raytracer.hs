module Raytracer
    ( trace
    ) where

import qualified Shape as S
import qualified Material as M
import qualified Vector as V
import Linear.V3 (V3 (..))
import Vector ((-*-))
import Material ((+^+), (*^*))

trace :: [(S.Shape, M.Material)] -> [M.Light] -> M.Color -> M.ReflectionStrategy -> Int -> V.Ray -> M.Color
trace shapes lights ambientLight _ 0 ray@(V.Ray origin direction) =
  case findIntersection shapes ray of
    Nothing -> M.Color 0 0 0
    Just (intersection, normal, material) -> M.reflectedLight material lights ambientLight normal origin intersection
trace shapes lights ambientLight reflectionStrategy maxBounces ray@(V.Ray origin direction) =
  case findIntersection shapes ray of
    Nothing -> M.Color 0 0 0
    Just (intersection, normal, material) ->
      let reflections = M.reflect reflectionStrategy direction normal
          outboundRays = map (V.Ray intersection) reflections
          subtraceColors = map (trace shapes lights ambientLight reflectionStrategy (maxBounces - 1)) outboundRays
      in foldr (+^+) (M.Color 0 0 0) subtraceColors

-- Find the (intersection, normal, material) corresponding to the first shape that this ray would hit
findIntersection :: [(S.Shape, M.Material)] -> V.Ray -> Maybe (V3 Double, V3 Double, M.Material)
findIntersection shapes ray@(V.Ray origin direction) = 
  let intersections = map extract $ filter isPresent $ map getData shapes
  in if null intersections then Nothing
     else let (firstIntersectionData:rest) = map getRatio intersections
              (intersection, normal, material, _) = foldr selectLower firstIntersectionData rest
          in Just (intersection, normal, material)
  where getData (shape, material) =
          let intersectData = S.intersectData shape ray
          in case intersectData of
              Just (intersection, normal) -> Just (intersection, normal, material)
              Nothing -> Nothing
        isPresent m = case m of
          Just _ -> True
          Nothing -> False
        extract (Just m) = m
        vecDiv (V3 x1 _ _ ) (V3 x2 _ _) = x1 / x2
        getRatio (point, normal, material) = (point, normal, material, vecDiv (normal -*- origin) direction)
        selectLower i1@(int1, norm1, mat1, ratio1) i2@(int2, norm2, mat2, ratio2) = if ratio1 < ratio2 then i1 else i2
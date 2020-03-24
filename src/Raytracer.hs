module Raytracer
    ( trace
    ) where

import qualified Shape as S
import qualified Material as M
import qualified Vector as V
import Linear.V3 (V3 (..))
import Vector ((.*.), (***), (+*+), (-*-))

trace :: [(S.Shape, M.Material)] -> [M.Light] -> M.Color -> V.Ray -> M.ReflectionStrategy -> Int -> M.Color
trace shapes lights ambientLight ray@(V.Ray origin direction) _ 0 =
  case findIntersection shapes ray of
    Nothing -> M.Color 0 0 0
    Just (intersection, normal, material) -> M.reflectedLight material lights ambientLight normal origin intersection
trace shapes lights ambientLight ray@(V.Ray origin direction) reflectionStrategy maxBounces =
  M.Color 0 0 0

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

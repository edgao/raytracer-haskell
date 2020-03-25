module Raytracer
    ( raytrace
    ) where

import qualified Shape as S
import qualified Material as M
import qualified Vector as V
import Linear.V3 (V3 (..))
import Vector ((-*-))
import Material ((+^+), (*^*))
import Data.Maybe (isNothing, mapMaybe)

raytrace :: [(S.Shape, M.Material)] -> [M.Light] -> M.Color -> M.ReflectionStrategy -> Int -> V.Ray -> M.Color
raytrace shapes lights ambientLight _ 0 ray@(V.Ray origin direction) =
  case findIntersection shapes ray of
    Nothing -> M.Color 0 0 0
    Just (intersection, normal, material) -> M.reflectedLight material visibleLights ambientLight normal origin intersection
      where visibleLights = findVisibleLights shapes intersection lights
raytrace shapes lights ambientLight reflectionStrategy maxBounces ray@(V.Ray origin direction) =
  case findIntersection shapes ray of
    Nothing -> M.Color 0 0 0
    Just (intersection, normal, material) ->
      let reflections = M.reflect reflectionStrategy direction normal
          outboundRays = map (V.Ray intersection) reflections
          subtraceColors = map (raytrace shapes lights ambientLight reflectionStrategy (maxBounces - 1)) outboundRays
          visibleLights = findVisibleLights shapes intersection lights
          hereColor = M.reflectedLight material visibleLights ambientLight normal origin intersection
      in foldr (+^+) (M.Color 0 0 0) subtraceColors +^+ hereColor

-- Find the (intersection, normal, material) corresponding to the first shape that this ray would hit
findIntersection :: [(S.Shape, M.Material)] -> V.Ray -> Maybe (V3 Double, V3 Double, M.Material)
findIntersection shapes ray@(V.Ray origin direction) = selectLowest getRatio $ mapMaybe getData shapes
  where getData (shape, material) = case S.intersectData ray shape of
          Nothing -> Nothing
          Just (intersection, normal) -> Just (intersection, normal, material)
        getRatio (intersection, normal, material) = (V.normSquare $ intersection -*- origin) / (V.normSquare direction)
        selectLower fn a b = if fn a < fn b then a else b
        selectLowest _ [] = Nothing
        selectLowest fn (a1:as) = Just $ foldr (selectLower fn) a1 as

findVisibleLights :: [(S.Shape, M.Material)] -> V3 Double -> [M.Light] -> [M.Light]
findVisibleLights shapes position = filter (isNothing . findIntersection shapes . V.Ray position . (-*- position) . M.location)

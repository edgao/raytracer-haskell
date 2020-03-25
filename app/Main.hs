module Main where

import qualified Data.ByteString.Lazy as BS (writeFile)
import Codec.Picture as P (PixelRGB8 (..), DynamicImage (ImageRGB8), generateImage)
import Codec.Picture.Saving as PS (imageToPng)
import Debug.Trace (trace)

import qualified Material as M
import qualified Shape as S
import qualified Raytracer as R
import Linear.V3 (V3 (..))
import qualified Vector as V
import Vector ((-*-))

main :: IO ()
main = do
  let width = 1000
  let height = 1000
  let camera = V3 0 0 10
  let
    fn :: Int -> Int -> PixelRGB8
    fn imgX imgY =
        let x = 4.0 * (fromIntegral imgX) / (fromIntegral width) - 2
            y = 2 - 4.0 * (fromIntegral imgY) / (fromIntegral height)
            intersection = sphereIntersection x y
            viewport = V3 x y 5
            (r, g, b) = M.render $ M.clampColor $ R.trace shapes lights ambientLight reflectionStrategy 8 (V.Ray camera (V.normalize $ viewport - camera))
        in P.PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)
  let img = P.generateImage fn width height
  BS.writeFile "/home/edgao/Desktop/test.png" $ PS.imageToPng $ P.ImageRGB8 img
  return ()

sphereIntersection :: Double -> Double -> Maybe (V3 Double)
sphereIntersection x y = if x * x + y * y > 1 then Nothing
  else Just $ V3 x y (sqrt (1 - x * x - y * y))

material = M.PhongMaterial (M.Color 0.05 0.2 0.2) (M.Color 0.3 0.5 0.5) (M.Color 0.5 0.9 0.9) 50
dullMaterial = M.PhongMaterial (M.Color 0.05 0.05 0.05) (M.Color 0.3 0.1 0.1) (M.Color 0 0 0) 50

shapes = [
    (
      S.Triangle (V3 (-5) (-5) (-5)) (V3 0 4 (-5)) (V3 5 (-5) (-5)),
      dullMaterial
    )
    ,
    (
      S.Sphere (V3 1.5 1.5 1) 1,
      material
    )
    ,
    (
      S.Sphere (V3 (-1.5) 0.5 0.5) 1,
      material
    )
    ,
    (
      S.Sphere (V3 0 (-1.5) 2) 1,
      material
    )
  ]

lights = [
    M.Light
      (M.Color 1 1 1)
      (V3 10 10 100)
  ]

ambientLight = M.Color 1 1 1

reflectionStrategy = M.Mirror

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
import Vector ((-*-), (***), (+*+))

main :: IO ()
main = do
  let fn imgX imgY =
        let viewport = imageCoordToWorldCoord imgX imgY
            (r, g, b) = M.render $ M.clampColor $ R.raytrace shapes lights ambientLight reflectionStrategy 8 (V.Ray camera (V.normalize $ viewport - camera))
        in P.PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)
  let img = P.generateImage fn width height
  BS.writeFile "/home/edgao/Desktop/test.png" $ PS.imageToPng $ P.ImageRGB8 img
  return ()

width = 5000
height = 5000

camera = V3 15 15 2

imageCoordToWorldCoord :: Int -> Int -> V3 Double
imageCoordToWorldCoord imgX imgY = viewportUL +*+ ((fromIntegral imgX / fromIntegral width) *** deltaX) +*+ ((fromIntegral imgY / fromIntegral height) *** deltaY)
  where viewportUL = V3 7.5 0 4
        viewportUR = V3 0 7.5 4
        viewportLL = V3 7.5 0 (-5)
        deltaX = viewportUR -*- viewportUL
        deltaY = viewportLL -*- viewportUL

shapes = [
    (
      S.Triangle (V3 (-5) (-5) (-6)) (V3 0 4 (-6)) (V3 5 (-5) (-6)),
      M.PhongMaterial (M.Color 0.05 0.05 0.05) (M.Color 0.3 0.1 0.3) (M.Color 0.5 0.2 0.5) 50
    )
    ,
    (
      S.Sphere (V3 0 0 (-5)) 1,
      M.PhongMaterial (M.Color 0.05 0.05 0.05) (M.Color 0.5 0.2 0.2) (M.Color 0.9 0.6 0.6) 50
    )
    ,
    (
      S.Sphere (V3 (-2) (-3) (-5)) 1,
      M.PhongMaterial (M.Color 0.05 0.05 0.05) (M.Color 0.2 0.5 0.2) (M.Color 0.6 0.9 0.6) 50
    )
    ,
    (
      S.Sphere (V3 2 (-3) (-5)) 1,
      M.PhongMaterial (M.Color 0.05 0.05 0.05) (M.Color 0.2 0.2 0.5) (M.Color 0.6 0.6 0.9) 50
    )
    ,
    (
      S.Sphere (V3 0 (-2) (-5)) 1,
      M.PhongMaterial (M.Color 0.05 0.05 0.05) (M.Color 0.2 0.2 0.2) (M.Color 0.6 0.6 0.6) 50
    )
  ]

lights = [
    M.Light
      (M.Color 1 1 1)
      (V3 10 0 1)
    ,
    M.Light
      (M.Color 1 1 1)
      (V3 10 0 (-10))
    ,
    M.Light
      (M.Color 1 1 1)
      (V3 (-10) (-10) 10)
  ]

ambientLight = M.Color 1 1 1

reflectionStrategy = M.Mirror

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Debug.Trace

import qualified Data.ByteString.Char8 as B
import System.Environment (getArgs)
import qualified Data.Yaml as Y
import qualified Config as C

import Codec.Picture as P (PixelRGB8 (..), DynamicImage (ImageRGB8), generateImage)
import Codec.Picture.Saving as PS (imageToPng)
import qualified Data.ByteString.Lazy as BS (writeFile)

import qualified Material as M
import qualified Raytracer as R
import qualified Vector as V
import Vector ((-*-), (***), (+*+), (.*.))
import Linear.V3 (cross)

main :: IO ()
main = do
  (configPath:(outputPath:args)) <- getArgs
  configStr <- B.readFile configPath
  (C.Config
      (C.World objects lights ambientLight)
      (C.Raytracer reflectionStrategy maxBounces)
      (C.Image
        imgWidth
        imgHeight
        (C.Camera cameraPosition cameraDirection)
        (C.Viewport viewportVertical viewportWidth viewportHeight viewportDistance)
      )
    ) <- Y.decodeThrow configStr :: IO C.Config
  let shapes = map C.toTuple objects
  let cameraDirectionNorm = V.normalize cameraDirection
      viewportUp = V.normalize $ viewportVertical -*- ((viewportVertical .*. cameraDirection) *** cameraDirectionNorm)
      viewportRight = V.normalize $ cross cameraDirection viewportUp
      viewportUpperLeft = cameraPosition +*+ (viewportDistance *** cameraDirectionNorm) +*+ ((viewportHeight / 2) *** viewportUp) -*- ((viewportWidth / 2) *** viewportRight)
  let fn imgX imgY =
        let viewport = viewportUpperLeft +*+ ((viewportWidth * fromIntegral imgX / fromIntegral imgWidth) *** viewportRight) -*- ((viewportHeight * fromIntegral imgY / fromIntegral imgHeight) *** viewportUp)
            (r, g, b) = M.render $ M.clampColor $ R.raytrace shapes lights ambientLight reflectionStrategy maxBounces (V.Ray cameraPosition (V.normalize $ viewport - cameraPosition))
        in P.PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)
  let img = P.generateImage fn imgWidth imgHeight
  BS.writeFile outputPath $ PS.imageToPng $ P.ImageRGB8 img

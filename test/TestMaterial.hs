module TestMaterial
    ( unitTests
    ) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Linear.V3

import Material as M

unitTests = testGroup "Material unit tests" [
    mirrorTests
  ]

mirrorTests = testGroup "Mirror material" [
    testCase "Simple reflection" $ assertEqual ""
      [(V3 0 (-1) 1, M.Color 0.15 0.25 0.35)]
      (M.hit (M.Mirror $ M.Color 0.15 0.25 0.35) (V3 0 (-1) (-1)) (V3 0 0 1))
  ]

module TestVector
    ( unitTests
    ) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Linear.V3
import Vector as V

unitTests = testGroup
  "Vector unit tests"
  [
    testCase "Dot product" $ assertEqual [] 32 (V.dot (V3 1 2 3) (V3 4 5 6)),
    testCase "Scale" $ assertEqual [] (V3 2 4 6) (V.scale 2 (V3 1 2 3)),
    testCase "Add" $ assertEqual [] (V3 5 7 9) (V.add (V3 1 2 3) (V3 4 5 6)),
    testCase "Simple reflection" $ assertEqual [] (V3 1 1 1) (V.reflect (V3 1 1 (-1)) (V3 0 0 1))
  ]

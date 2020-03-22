module TestShape
    ( unitTests
    ) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Linear.V3 (V3 (..))

import Shape as S

unitTests = testGroup "Shape unit tests" [
    triangleTests
  ]

triangleTests = testGroup "Triangle shape" [
    testCase "Calculate normal" $ assertEqual ""
      (V3 0 0 1)
      (normal (Triangle (V3 (-1) 0 0) (V3 0 1 0) (V3 1 0 0)) (V3 42 84 126))
  ]

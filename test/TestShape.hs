module TestShape
    ( unitTests
    ) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Linear.V3 (V3 (..))

import Shape as S

unitTests = testGroup "Shape" [
    triangleTests
  ]

triangleTests = testGroup "Triangle" [
    testCase "Basic intersection" $ assertEqual ""
      (Just (V3 0 0 0))
      (intersect (Triangle (V3 (-1) 0 0) (V3 0 1 0) (V3 1 0 0)) (V3 0 0 1) (V3 0 0 (-1))),
    testCase "Basic non-intersection" $ assertEqual  ""
      Nothing
      (intersect (Triangle (V3 (-1) 0 0) (V3 0 1 0) (V3 1 0 0)) (V3 2 2 1) (V3 0 0 (-1))),
    testCase "Basic parallel" $ assertEqual ""
      Nothing
      (intersect (Triangle (V3 (-1) 0 0) (V3 0 1 0) (V3 1 0 0)) (V3 2 2 1) (V3 1 1 0)),
    testCase "Basic line intersection" $ assertEqual ""
      Nothing
      (intersect (Triangle (V3 (-1) 0 0) (V3 0 1 0) (V3 1 0 0)) (V3 0 0 1) (V3 1 1 0)),
    testCase "Calculate normal" $ assertEqual ""
      (V3 0 0 1)
      (normal (Triangle (V3 (-1) 0 0) (V3 0 1 0) (V3 1 0 0)) (V3 42 84 126))
  ]

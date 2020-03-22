module TestShape
    ( unitTests
    ) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Linear.V3 (V3 (..))
import qualified Vector as V

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
      (intersect (Triangle (V3 0 1 0) (V3 1 0 0) (V3 0 0 1)) (V3 0 0 1) (V3 1 1 0)),
    testCase "Basic normal" $ assertEqual ""
      (V3 0 0 1)
      (normal (Triangle (V3 (-1) 0 0) (V3 0 1 0) (V3 1 0 0)) (V3 42 84 126)),
    testCase "Slanted edge intersection" $ assertEqual ""
      (Just (V3 0.5 0.5 0))
      (intersect (Triangle (V3 0 1 0) (V3 1 0 0) (V3 0 0 1)) (V3 0 0 0) (V3 1 1 0)),
    testCase "Slanted corner intersection" $ assertEqual ""
      (Just (V3 1 0 0))
      (intersect (Triangle (V3 0 1 0) (V3 1 0 0) (V3 0 0 1)) (V3 0 0 0) (V3 1 0 0)),
    testCase "Slanted non-intersection" $ assertEqual ""
      Nothing
      (intersect (Triangle (V3 0 1 0) (V3 1 0 0) (V3 0 0 1)) (V3 4 4 1) (V3 1 1 1)),
    testCase "Slanted parallel" $ assertEqual ""
      Nothing
      (intersect (Triangle (V3 0 1 0) (V3 1 0 0) (V3 0 0 1)) (V3 1 1 1) (V3 1 1 (-1))),
    testCase "Slanted line intersection" $ assertEqual ""
      Nothing
      (intersect (Triangle (V3 0 1 0) (V3 1 0 0) (V3 0 0 1)) (V3 0 0 1) (V3 1 1 (-1))),
    testCase "Slanted normal" $ assertEqual ""
      (V.normalize (V3 1 1 1))
      (normal (Triangle (V3 0 1 0) (V3 1 0 0) (V3 0 0 1)) (V3 42 84 126))
  ]

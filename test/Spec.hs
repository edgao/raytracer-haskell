import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import qualified TestVector as V
import qualified TestMaterial as M
import qualified TestShape as S

main = defaultMain $ testGroup "All tests" [
    V.unitTests,
    M.unitTests,
    S.unitTests
  ]

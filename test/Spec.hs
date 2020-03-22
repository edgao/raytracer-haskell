import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import qualified TestVector as V

main = defaultMain $ testGroup "Unit tests" [V.unitTests]

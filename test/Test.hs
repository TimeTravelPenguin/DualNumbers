import qualified AdditiveTests (tests)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck as QC ()

testTree :: TestTree
testTree = testGroup "Tests" [AdditiveTests.tests]

main :: IO ()
main = defaultMain testTree
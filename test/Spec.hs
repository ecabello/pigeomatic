module Main(main) where

import qualified Spec.Utils
import qualified Spec.GenoLib
import qualified Spec.Pigeon
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "pigeomatic" [
        Spec.Utils.tests,
        Spec.GenoLib.tests,
        Spec.Pigeon.tests
    ]
module Spec.Utils(tests) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Time.Calendar
import Data.Time.Clock
import Pigeomatic.Utils

tests :: TestTree
tests = testGroup "pigeomatic-utils" [ propertyTests, unitTests ]

propertyTests :: TestTree
propertyTests = testGroup "Property Tests" [
        -- toBin
        QC.testProperty "toBin produces 0s and 1s" $ \n -> all (\b -> b == 0 || b == 1) (toBin n),
        QC.testProperty "toBinBase n b produces an array of at least length b" $ 
            \n b -> 
                (b :: Int) >= 0 QC.==> length (toBinBase n b) >= b,
        QC.testProperty "toBin32 n produces an array of at least length 32" $ \n -> length (toBin32 n) >= 32,
        QC.testProperty "toBin64 n produces an array of length 64" $ \n -> length (toBin64 n) == 64,

        -- Random
        QC.testProperty "Verify randomInts n always produces array of length n if n > 0" $ 
            \s n -> 
                (n :: Int) > 0 QC.==> n == (length $ randomInts (mkRandom s) n),
        QC.testProperty "Verify skipRandom 0 (mkRandom s) == mkRandom s" $ 
            \s -> (fst $ skipRandom 0 (mkRandom s)) == (fst $ mkRandom s),
        QC.testProperty "Verify fst $ skipRandom n (mkRandom s) == last $ randomInts (mkRandom s) n" $ 
            \n s -> 
                (n :: Int) > 0 QC.==> (fst $ skipRandom n (mkRandom s)) == (last $ randomInts (mkRandom s) n),
        QC.testProperty "Verify pickRandom functionality for any random seed" $
            \s -> let rnd = mkRandom s
                      options = ['a'..'z']
                      sorter = randomInts rnd (length options)
                      winner = minimum $ sorter
                      Just winninIndex = elemIndex winner sorter
                      in (pickRandom rnd options) == (options !! winninIndex)

    ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [
        -- toBin
        testCase "toBin 0 should return [0]" $ toBin 0 @?= [0],
        testCase "toBin 1 should return [1]" $ toBin 1 @?= [1],
        testCase "toBin 2 should return [1,0]" $ toBin 2 @?= [1, 0],
        testCase "toBin 4 should return [1,0,0]" $ toBin 4 @?= [1, 0, 0],
        testCase "toBin 8 should return [1,0,0,0]" $ toBin 8 @?= [1, 0, 0, 0],
        testCase "toBin 16 should return [1,0,0,0,0]" $ toBin 16 @?= [1, 0, 0, 0, 0],
        testCase "toBin 32 should return [1,0,0,0,0,0]" $ toBin 32 @?= [1, 0, 0, 0, 0, 0],
        testCase "toBin 64 should return [1,0,0,0,0,0,0]" $ toBin 64 @?= [1, 0, 0, 0, 0, 0, 0],
        testCase "toBin 128 should return [1,0,0,0,0,0,0,0]" $ toBin 128 @?= [1, 0, 0, 0, 0, 0, 0, 0],
        testCase "toBin 256 should return [1,0,0,0,0,0,0,0,0]" $ toBin 256 @?= [1, 0, 0, 0, 0, 0, 0, 0, 0],
        testCase "toBin 512 should return [1,0,0,0,0,0,0,0,0,0]" $ toBin 512 @?= [1, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        testCase "toBin 1024 should return [1,0,0,0,0,0,0,0,0,0,0]" $ toBin 1024 @?= [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],

        -- Random
        testCase "Verify mkRandom 0 produces expected result" $ (fst $ mkRandom 0) @?= 2947667278772165694,
        testCase "Verify nextRandom produces expected result" $ (fst $ nextRandom $ mkRandom 0) @?= -144895307711186549,
        testCase "Verify randomInts 3 produces expected result" $ (randomInts (mkRandom 0) 3) @?= [-144895307711186549,729919693006235833,-7424912945573528338],

        -- Hash
        testCase "Verify hashSHA256 produces expected hash for Haskell" $ hashSHA256 "Haskell" @?= "540d2c45c091f2dfaa439dd63eff4c546699386ae3ac7e01c968e1825b615c63",
        testCase "Verify hashSHA256 produces expected hash for Pigeomatic" $ hashSHA256 "Pigeomatic" @?= "d5446ae21139e31bc82f5bb76b63927d7ab2f3b2de67e9ed23feeca31810df8a",
        testCase "Verify hashSHA256 produces expected hash for The quick brown fox jumps over the lazy dog" $
            hashSHA256 "The quick brown fox jumps over the lazy dog" @?= "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592",

        -- Iso8601
        testCase "Verify toIso8601 output" $ (toIso8601 $ UTCTime (fromGregorian 1970 1 1 :: Day) 0) @?= "1970-01-01T00:00:00Z",
        testCase "Verify toIso8601 output" $ (toIso8601 $ UTCTime (fromGregorian 2021 1 1 :: Day) 0) @?= "2021-01-01T00:00:00Z",
        testCase "Verify fromIso8601 output" $ (Just $ UTCTime (fromGregorian 1970 1 1 :: Day) 0) @?= fromIso8601 "1970-01-01T00:00:00Z",
        testCase "Verify fromIso8601 output" $ (Just $ UTCTime (fromGregorian 2021 1 1 :: Day) 0) @?= fromIso8601 "2021-01-01T00:00:00Z",

        -- merge
        testCase "Verify merge ouput" $ merge ['A', 'B', 'C'] ['a', 'b', 'c'] @?= ['A', 'a', 'B', 'b', 'C', 'c'],

        -- allEqual
        testCase "Verify allEqual returns True for empty list" $ allEqual ([] :: [Int]) @?= True,
        testCase "Verify allEqual returns True for simple list" $ (allEqual $ replicate 10 'a') @?= True,
        testCase "Verify allEqual returns False for infinite list of different numbers" $ allEqual [1..] @?= False
    ]
module Spec.Pigeon(tests) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Time.Calendar
import Data.Time.Clock
import Pigeomatic.Utils
import Pigeomatic.GenoLib
import Pigeomatic.Pigeon

tests :: TestTree
tests = testGroup "pigeomatic-pigeon" [ propertyTests, unitTests ]

propertyTests :: TestTree
propertyTests = testGroup "Property Tests" [
        QC.testProperty "Verify that all generated Pigeons are diploid organisms" $
            \seed -> let rnd = mkRandom seed
                         bday = UTCTime (fromGregorian 2021 1 1 :: Day) 0
                         in (isDiploid $ pigeonGenotype $ generateRandomPigeon rnd bday) && 
                            (isDiploid $ pigeonGenotype $ generateWildTypePigeon rnd bday),
        QC.testProperty "Verify that all generated Pigeons have correct ploidi number" $
            \seed -> let rnd = mkRandom seed
                         bday = UTCTime (fromGregorian 2021 1 1 :: Day) 0
                         in (ploidyNumber $ pigeonGenotype $ generateRandomPigeon rnd bday) == 4 &&
                            (ploidyNumber $ pigeonGenotype $ generateWildTypePigeon rnd bday) == 4,
        QC.testProperty "Verify pairPigeons produces correct offsprings" $
            \seed -> let rnd = mkRandom seed
                         dt = UTCTime (fromGregorian 2021 1 1 :: Day) 0
                         squeakers = mkTestPairing rnd dt 
                         in length squeakers == 2 &&
                            all (\p -> (isDiploid $ pigeonGenotype $ p) && 
                                (ploidyNumber $ pigeonGenotype $ p) == 4) squeakers

    ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [
        testCase "Verify Test population" $ do
            now <- getCurrentTime
            all (\p -> (ploidyNumber $ pigeonGenotype $ p) == 4) (mkTestPopulation' now 1000) @?= True,
        testCase "Verify Test population is alive one year later" $ do
            now <- getCurrentTime
            all (isPigeonAlive now) (mkTestPopulation' (oneYearAgoFrom now) 1000) @?= True,
        testCase "Verify Test population is sexually mature one year later" $ do
            now <- getCurrentTime
            all (isPigeonSexuallyMature now) (mkTestPopulation' (oneYearAgoFrom now) 1000) @?= True,
        testCase "Verify Test population Hens are hemizygous for the B locus" $ do
            now <- getCurrentTime
            (all (\p -> hemizygous $ alleleGenes (SLocus "B") $ 
                head $ filter (\cs -> not $ homologous cs ) $ genotypeChromosomes $ pigeonGenotype p) $ 
                    filter (\p -> HEN == pigeonSex p) (mkTestPopulation' now 1000)) @?= True            
    ]


oneYearAgoFrom :: UTCTime -> UTCTime
oneYearAgoFrom dt = addUTCTime (-365 * 24 * 60 *60) dt

mkTestPopulation :: Random -> UTCTime -> Int -> [Pigeon]
mkTestPopulation rnd bday n = map (\i -> generateRandomPigeon (skipRandom i rnd) bday) [1..n] 

mkTestPopulation' :: UTCTime -> Int -> [Pigeon]
mkTestPopulation' = mkTestPopulation (mkRandom 0)

mkTestPairing :: Random -> UTCTime -> [Pigeon]
mkTestPairing rnd dt = pairPigeons rnd dt mother father
    where Just father = find (\p -> pigeonSex p == COCK) population
          Just mother = find (\p -> pigeonSex p == HEN) population
          population = mkTestPopulation rnd (oneYearAgoFrom dt ) 1000 

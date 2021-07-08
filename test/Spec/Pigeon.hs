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
import Pigeomatic.PigeonDNA

tests :: TestTree
tests = testGroup "pigeomatic-pigeon" [ propertyTests, unitTests ]

propertyTests :: TestTree
propertyTests = testGroup "Property Tests" [
        QC.testProperty "Verify that all generated Pigeons are diploid organisms" $
            \seed -> let rnd = mkRandom seed
                         in (isDiploid $ pigeonGenotype $ generateRandomPigeon rnd arbitraryDate) && 
                            (isDiploid $ pigeonGenotype $ generateWildTypePigeon rnd arbitraryDate),
        QC.testProperty "Verify that all generated Pigeons have correct ploidi number" $
            \seed -> let rnd = mkRandom seed
                         in (ploidyNumber $ pigeonGenotype $ generateRandomPigeon rnd arbitraryDate) == pigeonPloidyNumber &&
                            (ploidyNumber $ pigeonGenotype $ generateWildTypePigeon rnd arbitraryDate) == pigeonPloidyNumber,
        QC.testProperty "Verify size is in expected range" $
            \seed -> let rnd = mkRandom seed
                         rps = pigeonSize $ generateRandomPigeon rnd arbitraryDate
                         wps = pigeonSize $ generateWildTypePigeon rnd arbitraryDate
                         in rps >= minPigeonSize && rps <= maxPigeonSize &&
                            wps >= minPigeonSize && wps <= maxPigeonSize,                                
        QC.testProperty "Verify longevity is in expected range" $
            \seed -> let rnd = mkRandom seed
                         rpl = pigeonLongevity $ generateRandomPigeon rnd arbitraryDate
                         wpl = pigeonLongevity $ generateWildTypePigeon rnd arbitraryDate
                         in rpl >= minPigeonLongevity && rpl <= maxPigeonLongevity &&
                            wpl >= minPigeonLongevity && wpl <= maxPigeonLongevity,
        QC.testProperty "Verify charisma is in expected range" $
            \seed -> let rnd = mkRandom seed
                         rpc = pigeonCharisma $ generateRandomPigeon rnd arbitraryDate
                         wpc = pigeonCharisma $ generateWildTypePigeon rnd arbitraryDate
                         in rpc >= minPigeonCharisma && rpc <= maxPigeonCharisma &&
                            wpc >= minPigeonCharisma && wpc <= maxPigeonCharisma,
        QC.testProperty "Verify loyalty is in expected range" $
            \seed -> let rnd = mkRandom seed
                         rpl = pigeonLoyalty $ generateRandomPigeon rnd arbitraryDate
                         wpl = pigeonLoyalty $ generateWildTypePigeon rnd arbitraryDate
                         in rpl >= minPigeonLoyalty && rpl <= maxPigeonLoyalty &&
                            wpl >= minPigeonLoyalty && wpl <= maxPigeonLoyalty,
        QC.testProperty "Verify pairPigeons produces correct offsprings" $
            \seed -> let rnd = mkRandom seed
                         squeakers = mkTestPairing rnd arbitraryDate 
                         in length squeakers == 2 &&
                            all (\p -> (isDiploid $ pigeonGenotype $ p) && 
                                (ploidyNumber $ pigeonGenotype $ p) == pigeonPloidyNumber) squeakers,
        QC.testProperty "Verify mature pigeons always win against early flyers" $ do
            \seed -> let rnd = mkRandom seed
                         mature = generateRandomPigeon rnd (oneYearAgoFrom arbitraryDate) 
                         eflyer = generateRandomPigeon (skipRandom 100 rnd) (threeMonthsAgoFrom arbitraryDate) 
                         Just winner = pigeonDuel arbitraryDate mature eflyer
                         in winner == mature
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
            (all (\p -> hemizygous $ alleleGenes (SLocus "B") $ head $ 
                filter (not . homologous) $ genotypeChromosomes $ pigeonGenotype p) $ 
                        filter (\p -> HEN == pigeonSex p) (mkTestPopulation' now 1000)) @?= True,
        testCase "Verify pigeonDuel produces the right winner" $ do
            now <- getCurrentTime
            let rnd = mkRandom 0
                ps = mkTestPopulation rnd (oneYearAgoFrom now) 1000
            all (\i -> let p1 = ps !! i
                           p2 = pickRandom (skipRandom i rnd) (filter (\p -> p /= p1) ps)
                           winner = pigeonDuel now p1 p2
                           in verifyDuel p1 p2 winner) [0..999] @?= True                        
    ]

pigeonPloidyNumber :: Int
pigeonPloidyNumber = 4

arbitraryDate :: UTCTime
arbitraryDate = UTCTime (fromGregorian 2021 1 1 :: Day) 0

oneYearAgoFrom :: UTCTime -> UTCTime
oneYearAgoFrom dt = addUTCTime (-365 * 24 * 60 *60) dt

threeMonthsAgoFrom :: UTCTime -> UTCTime
threeMonthsAgoFrom dt = addUTCTime (-90 * 24 * 60 *60) dt

mkTestPopulation :: Random -> UTCTime -> Int -> [Pigeon]
mkTestPopulation rnd bday n = map (\i -> generateRandomPigeon (skipRandom i rnd) bday) [1..n] 

mkTestPopulation' :: UTCTime -> Int -> [Pigeon]
mkTestPopulation' = mkTestPopulation (mkRandom 0)

mkTestPairing :: Random -> UTCTime -> [Pigeon]
mkTestPairing rnd dt = pairPigeons rnd dt mother father
    where Just father = find (\p -> pigeonSex p == COCK) population
          Just mother = find (\p -> pigeonSex p == HEN) population
          population = mkTestPopulation rnd (oneYearAgoFrom dt ) 1000 

verifyDuel :: Pigeon -> Pigeon -> Maybe Pigeon -> Bool
verifyDuel p1 p2 winner | winner == Just p1 = p1vsp2 > p2vsp1
                        | winner == Just p2 = p2vsp1 > p1vsp2
                        | otherwise = p1vsp2 == p2vsp1 || (p1vsp2 <= 0 && p2vsp1 <= 0)
                        where p1vsp2 = pigeonCharisma p1 - pigeonLoyalty p2
                              p2vsp1 = pigeonCharisma p2 - pigeonLoyalty p1
                              
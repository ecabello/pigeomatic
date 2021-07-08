{-# LANGUAGE OverloadedStrings #-}

-- | The organism that we are trying to model
module Pigeomatic.Pigeon (
    Pigeon(..),
    PigeonSex(..),
    pigeonSex,
    pigeonSize,
    pigeonLongevity,
    pigeonCharisma,
    pigeonLoyalty,
    isPigeonAlive,
    isPigeonSexuallyMature,
    canPigeonFly,
    generateWildTypePigeon,
    generateRandomPigeon,
    pairPigeons,
    computePigeonPhenotype,
    pigeonDuel    
) where

import Data.Char
import Data.List
import Data.Aeson
import Data.Time.Clock
import Pigeomatic.Utils
import Pigeomatic.GenoLib
import Pigeomatic.PigeonDNA

-- | Pigeons, they breed, they fly, they $#it everywhere and supposedly good at chess...
data Pigeon = Pigeon {
    pigeonId :: String,
    pigeonFather :: String,
    pigeonMother :: String,
    pigeonBirthday :: UTCTime,
    pigeonGenotype :: Genotype
} deriving (Show, Read)

instance Eq Pigeon where
    (==) p1 p2 = pigeonId p1 == pigeonId p2

instance ToJSON Pigeon where
    toJSON (Pigeon pigeonId pigeonFather pigeonMother pigeonBirthday pigeonGenotype ) =
        object [
            "id"       .= pigeonId, 
            "father"   .= pigeonFather,
            "mother"   .= pigeonMother,
            "birthday" .= toIso8601 pigeonBirthday, 
            "genotype" .= (pigeonGenotypeToCompactFormat pigeonGenotype)
        ]

instance FromJSON Pigeon where
    parseJSON = withObject "Pigeon" $ \obj -> do
        id <- obj .: "id"
        father <- obj .: "father"
        mother <- obj .: "mother"
        bday <- obj .: "birthday"
        genotype <- obj .: "genotype"
        let Just utc = fromIso8601 bday
        return ( Pigeon {pigeonId=id, pigeonFather=father, pigeonMother=mother, pigeonBirthday=utc, pigeonGenotype=pigeonGenotypeFromCompactFormat genotype})


-- | 'PigeonSex' Yes, that is what male pigeons are called. Females are Hens
data PigeonSex = COCK | HEN
    deriving (Eq, Show, Read, Enum)

-- | 'pigeonSex' determines the sex of the pigeon by looking at the genotype
pigeonSex :: Pigeon -> PigeonSex
pigeonSex p | null $ filter (not . homologous) $ genotypeChromosomes $ pigeonGenotype p = COCK
            | otherwise = HEN

-- | 'generateWildTypePigeon' generates a Blue bar pigeon
generateWildTypePigeon :: Random -> UTCTime -> Pigeon
generateWildTypePigeon rnd bday = Pigeon { 
        pigeonId = hashSHA256 (nonce ++ "::" ++ (toIso8601 bday) ++ show chromosomes),
        pigeonFather = "",
        pigeonMother = "",
        pigeonBirthday = bday,
        pigeonGenotype = Genotype { 
            genotypeChromosomes = chromosomes
        }
    }
    where chromosomes = generateWildTypeChromosomes rnd
          nonce = foldl (\acc i -> acc ++ (show i)) "" (randomInts rnd 3)

-- | 'generateRandomPigeon' generates a fancy looking pigeon
generateRandomPigeon :: Random -> UTCTime -> Pigeon
generateRandomPigeon rnd bday = Pigeon { 
        pigeonId = hashSHA256 (nonce ++ "::" ++ (toIso8601 bday) ++ show chromosomes),
        pigeonFather = "",
        pigeonMother = "",
        pigeonBirthday = bday,
        pigeonGenotype=Genotype {
            genotypeChromosomes= chromosomes
        }
    }
    where chromosomes = generateRandomChromosomes rnd
          nonce = foldl (\acc i -> acc ++ (show i)) "" (randomInts rnd 3)

pairingTime = 10 * 24 * 60 * 60                 -- 10 days pairing time before laying first egg
secondEggDelay = 1 * 24 * 60 * 60               -- 1 day between laying first egg and second
incubationTime = 18 * 24 * 60 * 60              -- 18 days incubation 
firstFlight = 42 - 24 * 60 * 60                 -- 6 weeks from birth until they master flying
sexualMaturityTime = 6 * 30 * 24 * 60 * 60      -- 6 months until they reach sexual maturity
maxLifespan = 15 * 365 * 24 * 60 * 60           -- 15 years maximum lifespan

-- | 'isPigeonAlive' determines if a pigeon is alive at the given time
isPigeonAlive :: UTCTime -> Pigeon -> Bool
isPigeonAlive dt p = (dt `diffUTCTime` bday) > 0 && (death `diffUTCTime` dt) > 0
    where death = addUTCTime (realToFrac (lngv * maxLifespan / 100.0)) bday
          lngv = pigeonLongevity p
          bday = pigeonBirthday p

-- | 'isPigeonSexuallyMature' determines if a pigeon is ready to breed at the given time
isPigeonSexuallyMature :: UTCTime -> Pigeon -> Bool
isPigeonSexuallyMature dt p = (dt `diffUTCTime` maturity) > 0
    where maturity = addUTCTime sexualMaturityTime (pigeonBirthday p)

canPigeonFly :: UTCTime -> Pigeon -> Bool
canPigeonFly dt p = (dt `diffUTCTime` fstFlight) > 0 && (death `diffUTCTime` dt) > 0
    where death = addUTCTime (realToFrac (lngv * maxLifespan / 100.0)) bday
          lngv = pigeonLongevity p
          fstFlight = addUTCTime firstFlight bday
          bday = pigeonBirthday p

-- | 'pairPigeons' given two pigeons of opposite sex, pairs them and returns two squeakers (pigeon babies)
pairPigeons :: Random -> UTCTime -> Pigeon -> Pigeon -> [Pigeon]
pairPigeons rnd dt p1 p2 
    | (pigeonSex p1 /= pigeonSex p2) && 
      (isPigeonAlive dt p1 && isPigeonAlive dt p2) && 
      (isPigeonSexuallyMature dt p1 && isPigeonSexuallyMature dt p2) = [
            mkPigeon p1Bday (generateGamete rnd $ pigeonGenotype mother) (generateGamete (skipRandom 1 rnd) $ pigeonGenotype father),
            mkPigeon p2Bday (generateGamete (skipRandom 2 rnd) $ pigeonGenotype mother) (generateGamete (skipRandom 3 rnd) $ pigeonGenotype father)
        ]
    | otherwise = [] 
    where 
          fatherId = pigeonId father
          motherId = pigeonId mother
          father | pigeonSex p1 == COCK = p1
                 | otherwise = p2
          mother | pigeonSex p1 == HEN = p1
                 | otherwise = p2
          p1Bday = addUTCTime (pairingTime + incubationTime) dt
          p2Bday = addUTCTime (pairingTime + secondEggDelay + incubationTime) dt
          mkPigeon bday g1 g2 = Pigeon { 
                pigeonId = hashSHA256 (fatherId ++ ":" ++ motherId ++ ":" ++ (toIso8601 bday) ++ (show $ genotypeChromosomes genotype)),
                pigeonFather = fatherId,
                pigeonMother = motherId,
                pigeonBirthday = bday,
                pigeonGenotype= genotype
            }
            where genotype = combineGametes g1 g2

-- | 'computePigeonPhenotype' computes a pigeon's phenotype
computePigeonPhenotype :: Pigeon -> Phenotype
computePigeonPhenotype = computePhenotype (map (\(_, l, h) -> (l,h)) pigeonTraits) . pigeonGenotype


-- | 'pigeonDuel' given two pigeons, it pits their charisma against their loyalty
-- and determines a winner. If both pigeons have more loyalty than their opponent's
-- charimas the duel ends in a draw and returns Nothing.
-- Mature pigeons have the advantage of experience gainst new fliers but new fliers
-- compete at the same level
pigeonDuel :: UTCTime -> Pigeon -> Pigeon -> Maybe Pigeon
pigeonDuel dt p1 p2 
    | canPigeonFly dt p1 && canPigeonFly dt p2 =
        if p1Mature && not p2Mature then
            Just p1
        else 
            if p2Mature && not p1Mature then
                Just p2
            else
                if p1vsp2 /= p2vsp1 && (p1vsp2 > 0 || p2vsp1 > 0) then
                    if p1vsp2 > p2vsp1 then
                        Just p1
                    else
                        Just p2
                else 
                    Nothing
    | otherwise = Nothing
        where p1Mature = isPigeonSexuallyMature dt p1
              p2Mature = isPigeonSexuallyMature dt p2
              p1vsp2 = pigeonCharisma p1 - pigeonLoyalty p2
              p2vsp1 = pigeonCharisma p2 - pigeonLoyalty p1


-- | 'pigeonSize' calculates the effective size
pigeonSize :: Pigeon -> Double
pigeonSize p = pigeonEffectiveTraitValue (SLocus "SIZE") c3
    where c3 = (genotypeChromosomes $ pigeonGenotype p) !! 2

-- | 'pigeonLongevity' calculates the effective size
pigeonLongevity :: Pigeon -> Double
pigeonLongevity p = pigeonEffectiveTraitValue (SLocus "LNGV") c3
    where c3 = (genotypeChromosomes $ pigeonGenotype p) !! 2

-- | 'pigeonCharisma' calculates the effective charisma
pigeonCharisma :: Pigeon -> Double
pigeonCharisma p = pigeonEffectiveTraitValue (SLocus "CHRM") c3
    where c3 = (genotypeChromosomes $ pigeonGenotype p) !! 2

-- | 'pigeonLoyalty' calculates the effective loyalty
pigeonLoyalty :: Pigeon -> Double
pigeonLoyalty p = pigeonEffectiveTraitValue (SLocus "LOYT") c3
    where c3 = (genotypeChromosomes $ pigeonGenotype p) !! 2



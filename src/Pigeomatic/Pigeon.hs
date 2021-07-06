{-# LANGUAGE OverloadedStrings #-}

-- | The organism that we are trying to model
module Pigeomatic.Pigeon (
    Pigeon(..),
    PigeonSex(..),
    pigeonSex,
    isPigeonAlive,
    isPigeonSexuallyMature,
    generateWildTypePigeon,
    generateRandomPigeon,
    pairPigeons,
    computePigeonPhenotype
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
pigeonSex p | homologous $ last $ genotypeChromosomes $ pigeonGenotype p = COCK
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
    where chromosomes = generateWildTypeChromosomes $ skipRandom 1 rnd
          nonce = show $ fst $ nextRandom rnd

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
    where chromosomes = generateRandomChromosomes $ skipRandom 1 rnd
          nonce = show $ fst $ nextRandom rnd

pairingTime = 10 * 24 * 60 * 60                 -- 10 days pairing time before laying first egg
secondEggDelay = 1 * 24 * 60 * 60               -- 1 day between laying first egg and second
incubationTime = 18 * 24 * 60 * 60              -- 18 days incubation 
sexualMaturityTime = 6 * 30 * 24 * 60 * 60      -- 6 months until they reach sexual maturity 
maxLifespan = 15 * 365 * 24 * 60 * 60           -- 15 yeas maximum lifespan

isPigeonAlive :: UTCTime -> Pigeon -> Bool
isPigeonAlive t p = (t `diffUTCTime` bday) > 0 && (death `diffUTCTime` t) > 0
    where death = addUTCTime (realToFrac (lngv * maxLifespan / 100.0)) bday
          lngv = total / (fromIntegral $ length gs)
          total = foldl (\acc g -> acc + (unBlob $ geneBlob g)) 0 gs
          gs = alleleGenes (SLocus "LNGV") ((genotypeChromosomes $ pigeonGenotype p) !! 2)
          bday = pigeonBirthday p
          unBlob b = case b of
              NumericBlob x -> x
              otherwise -> 0

isPigeonSexuallyMature :: UTCTime -> Pigeon -> Bool
isPigeonSexuallyMature t p = (t `diffUTCTime` maturity) > 0
    where maturity = addUTCTime sexualMaturityTime (pigeonBirthday p)


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


{-
pigeonPhenotypeToXml :: Phenotype -> String
pigeonPhenotypeToXml p = "<?xml version=\"1.0\" encoding=\"utf-8\"?><Phenotype Organism=\"Pigeon\">" ++ (traitsToXml $ phenotypeTraits p) ++ "</Phenotype>"
    where
        traitsToXml :: [Trait] -> String
        traitsToXml ts = "<Traits>" ++ (foldl (\acc t ->( acc ++ ("<Trait Id=\"" ++ (id t) ++ "\" Name=\"" ++ (name t) ++ "\">" ++ (desc t) ++ (dat t) ++ "</Trait>"))) "" ts) ++ "</Traits>"
            where id = map toUpper . traitName
                  name t = traitName t
                  desc t = "<Description><![CDATA[" ++ (traitDescription t) ++ "]]></Description>"
                  dat t | traitBlob t == NullBlob = "" 
                        | otherwise = unBlob t
                    where unBlob t = case (traitBlob t) of
                            NumericBlob x -> "<Data Class=\"Number\">" ++ (show x)  ++ "</Data>"
                            StringBlob x -> "<Data Class=\"String\">" ++ x ++ "</Data>"
                            otherwise -> ""
-}                            
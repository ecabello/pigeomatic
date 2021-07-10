-- | Simplified pigeon genetics. 
-- Some resemblance to real pigeon genetics, a complex subject in its own right, 
-- were modeled in a very informal way. A real pigeon has 80 Chromosomes organized
-- in 40 pairs. Pigeomatic pigeons only have 8 chromosomes organized in 4 pairs
module Pigeomatic.PigeonDNA (
    maxPigeonSize, 
    minPigeonSize,
    maxPigeonLongevity, 
    minPigeonLongevity,
    maxPigeonLoyalty,
    minPigeonLoyalty,
    maxPigeonCharisma,
    minPigeonCharisma,
    pigeonTraits,
    pigeonGenotypeToCompactFormat,
    pigeonGenotypeFromCompactFormat,    
    generateWildTypeChromosomes,
    generateRandomChromosomes,
    pigeonEffectiveTraitValue
) where

import Data.List (find, maximumBy)
import Data.Ord

import Pigeomatic.Utils
import Pigeomatic.GenoLib

maxPigeonSize = 100.0
minPigeonSize = 85.0                    -- Size variation is only 15%
    
maxPigeonLongevity = 100.0
minPigeonLongevity = 75.0               -- No less than 3/4 of the lifespan of the ideal
    
maxPigeonLoyalty = 100.0
minPigeonLoyalty = 10.0
    
maxPigeonCharisma = 100.0
minPigeonCharisma = 10.0

-- Base Pigments
brownAllele = Gene { geneSymbol="b", geneDescription="Brown", geneDominance=(-100), geneBlob=StringBlob "-brown" }
blackAllele = Gene { geneSymbol="B(+)", geneDescription="Blue/Black", geneDominance=0, geneBlob=StringBlob "-blueblack" }
ashRedAllele = Gene { geneSymbol="B(A)", geneDescription="Ash Red", geneDominance=100, geneBlob=StringBlob "-ashred" }
pigmentAlleles = [brownAllele, blackAllele, ashRedAllele]

-- Wing patterns
barlessAllele = Gene { geneSymbol="c", geneDescription="Barless", geneDominance=(-100), geneBlob=NullBlob };
barAllele = Gene { geneSymbol="C(+)", geneDescription="Bar", geneDominance=0, geneBlob=StringBlob "-bar" };
lightCheckerAllele = Gene { geneSymbol="C(L)", geneDescription="Light Checker", geneDominance=100, geneBlob=StringBlob "-ltchecker" };
darkCheckerAllele = Gene { geneSymbol="C(D)", geneDescription="Dark Checker", geneDominance=200, geneBlob=StringBlob "-dkchecker" };
tPatternAllele = Gene { geneSymbol="C(T)", geneDescription="T-Pattern", geneDominance=300, geneBlob=StringBlob "-tpattern" };
patternAlleles = [barlessAllele, barAllele, lightCheckerAllele, darkCheckerAllele, tPatternAllele];
        
-- Spreads the color of the tail's bar to the whole body
noSpreadAllele = Gene { geneSymbol="s(+)", geneDescription="Wild Type", geneDominance=0, geneBlob=NullBlob };
spreadAllele = Gene { geneSymbol="S", geneDescription="Spread", geneDominance=100, geneBlob=StringBlob "-spread" };
spreadAlleles = [noSpreadAllele, spreadAllele]
        
-- Messes with the coloring of the feathers  
recessiveWhiteAllele = Gene { geneSymbol="z(wh)", geneDescription="Recessive White", geneDominance=(-400), geneBlob=StringBlob "-recwhite" };  -- White
tailMarkAllele = Gene { geneSymbol="z(tm)", geneDescription="Tail Mark", geneDominance=(-300), geneBlob=StringBlob "-tmark" };                 -- Colors Tail only
penciledAllele = Gene { geneSymbol="z(pc)", geneDescription="Penciled", geneDominance=(-200), geneBlob=StringBlob "-penciled" };               -- Colors Head and tail only
gazziAllele = Gene { geneSymbol="z", geneDescription="Gazzi", geneDominance=(-100), geneBlob=StringBlob "-gazzi" };                            -- Colors Head, wing shields and tail only
noGazziAllele = Gene { geneSymbol="Z(+)", geneDescription="Wild Type", geneDominance=0, geneBlob=NullBlob };
gazziAlleles = [recessiveWhiteAllele, tailMarkAllele, penciledAllele, gazziAllele, noGazziAllele]
        
-- Dilutes the base pigment
dilutedAllele = Gene { geneSymbol="d", geneDescription="Diluted", geneDominance=(-200), geneBlob=StringBlob "-diluted" };                      -- Cuts pigment by 50%
paleAllele = Gene { geneSymbol="d(P)", geneDescription="Pale", geneDominance=(-100), geneBlob=StringBlob "-pale" };                            -- Cuts pigment by 25%
noDilutionAllele = Gene { geneSymbol="D(+)", geneDescription="Wild Type", geneDominance=0, geneBlob=NullBlob };
dilutionAlleles = [dilutedAllele, paleAllele, noDilutionAllele]
    
-- Messes with the individual coloring of the feathers
noGrizzleAllele = Gene { geneSymbol="g(+)", geneDescription="Wild Type", geneDominance=0, geneBlob=NullBlob };
grizzleAllele = Gene { geneSymbol="G", geneDescription="Grizzle", geneDominance=100, geneBlob=StringBlob "-grizzle" };
tigerGrizzleAllele = Gene { geneSymbol="G(T)", geneDescription="Tiger Grizzle", geneDominance=200, geneBlob=StringBlob "-tgrizzle" };
grizzleAlleles = [noGrizzleAllele, grizzleAllele, tigerGrizzleAllele]
    
-- Creates white splashes in certain areas
pieBaldAllele = Gene { geneSymbol="pi(Bh)", geneDescription="Piebald", geneDominance=(-100), geneBlob=StringBlob "-piebald" };
crescentAllele = Gene { geneSymbol="pi(Cr)", geneDescription="Crescent", geneDominance=(-100), geneBlob=StringBlob "-crescent" };
whiteFlightsAllele = Gene { geneSymbol="pi(Wf)", geneDescription="White Flights", geneDominance=(-100), geneBlob=StringBlob "-whiteflights" };
whiteTailAllele = Gene { geneSymbol="pi(Wt)", geneDescription="White Tail", geneDominance=(-100), geneBlob=StringBlob "-whitetail" };
noPiedAllele = Gene { geneSymbol="pi(+)", geneDescription="Wild Type", geneDominance=0, geneBlob=NullBlob };
piedAlleles = [pieBaldAllele, crescentAllele, whiteFlightsAllele, noPiedAllele]

-- Recessive Red
recessiveRedAllele = Gene { geneSymbol="e", geneDescription="Recessive Red", geneDominance=(-100), geneBlob=StringBlob "-rred" };
noRecessiveRedAllele = Gene { geneSymbol="E(+)", geneDescription="WildType", geneDominance=0, geneBlob=NullBlob };
recessiveRedAlleles = [recessiveRedAllele, noRecessiveRedAllele]


-- (TraitName, Locus, Handler) table
pigeonTraits = [
        ("Pigment", SLocus "B", codominanceHandler),
        ("Pattern", SLocus "C", codominanceHandler),
        ("Spread", SLocus "S", codominanceHandler),
        ("Gazzi", SLocus "Z", codominanceHandler),
        ("Dilution", SLocus "D", codominanceHandler),
        ("Grizzle", SLocus "G", codominanceHandler),
        ("Pied", SLocus "PI", codominanceHandler),
        ("RRed", SLocus "E", codominanceHandler),
        ("Size", SLocus "SIZE", averagingHandler),
        ("Longevity", SLocus "LNGV", averagingHandler),
        ("Max Loyalty", SLocus "LOYT", averagingHandler),
        ("Max Charisma", SLocus "CHRM", averagingHandler)
    ]


-- | 'pigeonEffectiveTraitValue' calculates the effective trait value
pigeonEffectiveTraitValue :: Locus -> [Chromosome] -> Double          
pigeonEffectiveTraitValue l cs = total / (fromIntegral $ length gs)
    where gs = alleleGenes l cs
          total = foldl (\acc g -> acc + (unBlob $ geneBlob g)) 0 gs
          unBlob b = case b of
              NumericBlob x -> x
              otherwise -> 0    

traitFromLocus :: Locus -> String
traitFromLocus l = case (find (\(_,x,_) -> x == l) pigeonTraits) of
                       Just (t,_,_) -> t
                       Nothing -> "(UNK)"

codominanceHandler :: Locus -> [Gene] -> [Trait]
codominanceHandler l gs = traitFromGenes $ dominants gs
    where traitFromGenes (g:gs) = Trait { traitName=( traitFromLocus l), traitDescription=(traitDesc g), traitBlob=(geneBlob g)} : traitFromGenes gs
          traitFromGenes [] = []    
          traitDesc g = (inheritance gs) ++ " " ++ (geneDescription g) ++ " [" ++ (geneSymbol g) ++ "]"
          dominants gs = filter (\g -> geneDominance g == maxDominance gs) gs
          maxDominance = geneDominance . maximumBy (comparing geneDominance )
          inheritance gs | homozygous gs = "Homozygous"
                         | hemizygous gs = "Hemizygous"
                         | otherwise = "Heterozygous"

averagingHandler :: Locus -> [Gene] -> [Trait]
averagingHandler l gs = [
        Trait { 
            traitName=( traitFromLocus l), 
            traitDescription=((traitFromLocus l) ++ " " ++ (show $ average gs )), 
            traitBlob=(NumericBlob $ average gs)
        }
    ]
    where average gs = total / (fromIntegral $ length gs)
          total = foldl (\acc g -> acc + (unBlob $ geneBlob g)) 0 gs
          unBlob b = case b of
              NumericBlob x -> x
              otherwise -> 0


pigeonGenotypeToCompactFormat :: Genotype -> [[[String]]]
pigeonGenotypeToCompactFormat p = map (\grp -> map unlocus grp) (genotypeChromosomes p )
        where unlocus c = map (\(_,g) -> geneSymbol g) (chromosomeGenes c)

pigeonGenotypeFromCompactFormat :: [[[String]]] -> Genotype
pigeonGenotypeFromCompactFormat s = Genotype {
        genotypeChromosomes = [[
                mkC1Chromosome (gene 0 0 0) (gene 0 0 1) (gene 0 0 2),
                mkC1Chromosome (gene 0 1 0) (gene 0 1 1) (gene 0 1 2)
            ], [
                mkC2Chromosome (gene 1 0 0) (gene 1 0 1) (gene 1 0 2),
                mkC2Chromosome (gene 1 1 0) (gene 1 1 1) (gene 1 1 2)
            ], [
                mkC3Chromosome 
                    (getScalar $ symbol 2 0 0) (getScalar $ symbol 2 0 1) 
                    (getScalar $ symbol 2 0 2) (getScalar $ symbol 2 0 3),
                mkC3Chromosome 
                    (getScalar $ symbol 2 1 0) (getScalar $ symbol 2 1 1) 
                    (getScalar $ symbol 2 1 2) (getScalar $ symbol 2 1 3)
            ], [
                (c4 s),
                mkC4Chromosome (gene 3 1 0) (gene 3 1 1)
        ]]
    }
    where symbol grp ch g = ((s !! grp) !! ch) !! g
          gene grp ch g = geneFromSymbol $ symbol grp ch g
          isFemale s = any (\g -> length g == 0) (s !! 3)
          c4 s | isFemale s = mkSlugChromosome
               | otherwise = mkC4Chromosome (gene 3 0 0) (gene 3 0 1)
          getScalar s = read (drop 4 s) :: Double 

generateWildTypeChromosomes :: Random -> [[Chromosome]]
generateWildTypeChromosomes rnd = [[
        mkWildTypeC1Chromosome,
        mkWildTypeC1Chromosome
    ], [
        mkWildTypeC2Chromosome,
        mkWildTypeC2Chromosome
    ], [
        mkRandomC3Chromosome rnd,
        mkRandomC3Chromosome (skipRandom 50 rnd)
    ], [
        (pickRandom ( skipRandom 100 rnd ) [
            mkWildTypeC4Chromosome,
            mkSlugChromosome 
        ]),
        mkWildTypeC4Chromosome
    ]]

generateRandomChromosomes :: Random -> [[Chromosome]]
generateRandomChromosomes rnd = [[
        mkRandomC1Chromosome rnd,
        mkRandomC1Chromosome (skipRandom 50 rnd)
    ], [
        mkRandomC2Chromosome (skipRandom 100 rnd),
        mkRandomC2Chromosome (skipRandom 150 rnd)
    ], [
        mkRandomC3Chromosome (skipRandom 200 rnd),
        mkRandomC3Chromosome (skipRandom 250 rnd)
    ], [
        (pickRandom ( skipRandom 350 rnd ) [
            mkRandomC4Chromosome (skipRandom 400 rnd),
            mkSlugChromosome 
        ]),
        mkRandomC4Chromosome (skipRandom 300 rnd)
    ]]


mkWildTypeC1Chromosome :: Chromosome
mkWildTypeC1Chromosome = 
    mkC1Chromosome 
        (getWildTypeGene patternAlleles) 
        (getWildTypeGene grizzleAlleles) 
        (getWildTypeGene piedAlleles)

mkWildTypeC2Chromosome :: Chromosome
mkWildTypeC2Chromosome = 
    mkC2Chromosome
        (getWildTypeGene spreadAlleles) 
        (getWildTypeGene gazziAlleles)
        (getWildTypeGene recessiveRedAlleles)

mkWildTypeC4Chromosome :: Chromosome
mkWildTypeC4Chromosome = 
    mkC4Chromosome
        (getWildTypeGene pigmentAlleles)
        (getWildTypeGene dilutionAlleles)

mkRandomC1Chromosome :: Random -> Chromosome
mkRandomC1Chromosome rnd = 
    mkC1Chromosome
        (pickRandom rnd patternAlleles)
        (pickRandom rnd1 (mkRarityList 20 grizzleAlleles))
        (pickRandom rnd2 (mkRarityList 10 piedAlleles))
    where rnd1 = skipRandom (length patternAlleles)  rnd
          rnd2 = skipRandom (length grizzleAlleles)  rnd1

mkRandomC2Chromosome :: Random -> Chromosome
mkRandomC2Chromosome rnd = 
    mkC2Chromosome
        (pickRandom rnd spreadAlleles)
        (pickRandom rnd1 (mkRarityList 10 gazziAlleles)) 
        (pickRandom rnd2 (mkRarityList 5 recessiveRedAlleles))
    where rnd1 = skipRandom (length spreadAlleles)  rnd
          rnd2 = skipRandom (length gazziAlleles)  rnd1

mkRandomC3Chromosome :: Random -> Chromosome
mkRandomC3Chromosome rnd = mkC3Chromosome 
    ( rndValueR (minPigeonSize, maxPigeonSize) rnd )
    ( rndValueR (minPigeonLongevity, maxPigeonLongevity) (skipRandom 1 rnd)) 
    ( rndValueR (minPigeonLoyalty, maxPigeonLoyalty) (skipRandom 2 rnd)) 
    ( rndValueR (minPigeonCharisma, maxPigeonCharisma) (skipRandom 3 rnd)) 

mkRandomC4Chromosome :: Random -> Chromosome
mkRandomC4Chromosome rnd = 
    mkC4Chromosome
        (pickRandom rnd pigmentAlleles)
        (pickRandom rnd1 (mkRarityList 10 dilutionAlleles))
    where rnd1 = skipRandom (length pigmentAlleles)  rnd


mkC1Chromosome :: Gene -> Gene -> Gene -> Chromosome
mkC1Chromosome c g p= Chromosome {
        chromosomeGenes=[
            (SLocus "C", c), 
            (SLocus "G", g), 
            (SLocus "PI", p)
        ]
    }

mkC2Chromosome :: Gene -> Gene -> Gene -> Chromosome
mkC2Chromosome s z e = Chromosome {
        chromosomeGenes=[
            (SLocus "S", s), 
            (SLocus "Z", z),
            (SLocus "E", e)
        ]
    }
 
mkC3Chromosome size longevity loyalty charisma =
    Chromosome {
        chromosomeGenes=[
            (SLocus "SIZE", Gene {
                geneSymbol=("SIZE" ++ show size), 
                geneDescription=("Size " ++ show size),
                geneDominance=0,
                geneBlob=NumericBlob size
            }),
            (SLocus "LNGV", Gene {
                geneSymbol=("LNGV" ++ show longevity), 
                geneDescription=("Longevity " ++ show longevity),
                geneDominance=0,
                geneBlob=NumericBlob longevity
            }),
            (SLocus "LOYT", Gene {
                geneSymbol=("LOYT" ++ show loyalty), 
                geneDescription=("Loyalty " ++ show loyalty),
                geneDominance=0,
                geneBlob=NumericBlob loyalty
            }),
            (SLocus "CHRM", Gene {
                geneSymbol=("CHRM" ++ show charisma), 
                geneDescription=("Charisma " ++ show charisma),
                geneDominance=0,
                geneBlob=NumericBlob charisma
            })
        ]
    }

mkC4Chromosome :: Gene -> Gene -> Chromosome
mkC4Chromosome b d = Chromosome {
        chromosomeGenes=[
            (SLocus "B", b), 
            (SLocus "D", d)
        ]
    }    

mkSlugChromosome :: Chromosome
mkSlugChromosome = Chromosome {
        chromosomeGenes=[]
    }


geneFromSymbol :: String -> Gene
geneFromSymbol s = case find (\g -> s == geneSymbol g) allGenes of
                        Just g -> g
    where allGenes = pigmentAlleles ++ patternAlleles ++ spreadAlleles ++
                     gazziAlleles ++ dilutionAlleles ++ grizzleAlleles ++
                     piedAlleles ++ recessiveRedAlleles

getWildTypeGene :: [Gene] -> Gene
getWildTypeGene as = case (find (\g -> geneDominance g == 0) as) of
                        Just g -> g

rndValueR :: (Double, Double) -> Random -> Double
rndValueR (a, b) rnd = a + fromIntegral (next `mod` (truncate $ b-a)) 
    where next = abs $ fst $ nextRandom rnd

mkRarityList :: Int -> [Gene] -> [Gene]       
mkRarityList p as = merge ws rs
     where x = fromIntegral (p * l ) / 10.0
           l  = length as
           ws = replicate (round (fromIntegral l * 10 - x)) (getWildTypeGene as)
           rgs = filter (\g -> geneDominance g /= 0) as
           rs = map (\n -> rgs !! (n `mod` (length rgs))) [1..(round x)]

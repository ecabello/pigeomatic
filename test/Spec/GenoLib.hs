module Spec.GenoLib(tests) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Time.Calendar
import Data.Time.Clock
import Pigeomatic.Utils
import Pigeomatic.GenoLib

tests :: TestTree
tests = testGroup "pigeomatic-genolib" [ propertyTests, unitTests ]

propertyTests :: TestTree
propertyTests = testGroup "Property Tests" [
        QC.testProperty "loci should return expected gene locations" $ 
            \l -> (loci $ mkDummyChromosome l) == mkLocusList l,
        QC.testProperty "locusIn should find locus present in Chromosome" $ 
            \l -> 
                (l :: Int) > 3  QC.==> let c = mkDummyChromosome l
                                            in all (\i -> locusIn (ILocus i) c) [1..l],
        QC.testProperty "locusIn should not find a locus not present in Chromosome" $ 
            \l -> 
                (l :: Int) > 0  QC.==> locusIn (ILocus (l+1)) (mkDummyChromosome l) == False,
        QC.testProperty "ploidyNumber should return the number of chromosome sets in a Haploid Genotype" $ 
            \s -> 
                (s :: Int) > 0 QC.==> (ploidyNumber $ mkDummyHaploidGenotype s) == s,
        QC.testProperty "ploidyNumber should return the number of chromosome sets in a Diploid Genotype" $ 
            \s -> 
                (s :: Int) > 0 QC.==> (ploidyNumber $ mkDummyDiploidGenotype s) == s,
        QC.testProperty "ploidyNumber should return the number of chromosome sets in a Polyploid Genotype" $ 
            \s c -> 
                (s :: Int) > 0 && (c :: Int) > 2 QC.==> (ploidyNumber $ mkDummyPolyploidGenotype s c) == s,
        QC.testProperty "chromosomeCount should return the number of chromosomes in a Haploid Genotype" $ 
            \s -> 
                (s :: Int) > 0 QC.==> (chromosomeCount $ mkDummyHaploidGenotype s) == s,
        QC.testProperty "chromosomeCount should return the number of chromosomes in a Diploid Genotype" $ 
            \s -> 
                (s :: Int) > 0 QC.==> (chromosomeCount $ mkDummyDiploidGenotype s) == s * 2,  
        QC.testProperty "chromosomeCount should return the number of chromosomes in a Polyploid Genotype" $ 
            \s c-> 
                (s :: Int) > 0 && (c :: Int) > 2 QC.==> (chromosomeCount $ mkDummyPolyploidGenotype s c) == s * c,
        QC.testProperty "Verify alleGenes outputs the correct genes" $ 
            \l -> 
                (l :: Int) > 0 QC.==> let cset = [mkDummyChromosome l, mkDummyChromosome l]
                                        in all (\i -> allEqual $ alleleGenes (ILocus i) cset) [1..l],
        QC.testProperty "homologous should return True if chromosomes have the same loci" $ 
            \l c-> 
                (l :: Int) > 0 && (c :: Int) > 1 QC.==> let cset = replicate c (mkDummyChromosome l)
                                        in homologous cset,
        QC.testProperty "homologous should return False if chromosomes have different loci" $ 
            \l -> 
                (l :: Int) > 0 QC.==> False == homologous [mkDummyChromosome l, mkDummyChromosome $ l + 1],
        QC.testProperty "hemizygous should return False if there is more than one gene" $ 
            \n -> 
                (n :: Int) > 1 QC.==> False == (hemizygous $ replicate n (mkDummyGene 1)),
        QC.testProperty "homozygous should return True if all genes are the same" $ 
            \n g -> 
                (n :: Int) > 1 QC.==> homozygous $ replicate n (mkDummyGene g),
        QC.testProperty "heterozygous should return True if at least one gene is diferent" $ 
            \n g -> 
                (n :: Int) > 1 && (g :: Int) > 1 QC.==> heterozygous $ mkDummyGene 0 : replicate n (mkDummyGene g),
        QC.testProperty "heterozygous should return False if all genes are the same" $ 
            \n g -> 
                (n :: Int) > 1 && (g :: Int) > 1 QC.==> False == (heterozygous $ replicate n (mkDummyGene g)),
        QC.testProperty "isHaploid should return True for haploid genotype" $ 
            \s -> 
                (s :: Int) > 1 QC.==> isHaploid $ mkDummyHaploidGenotype s,         
        QC.testProperty "isHaploid should return False for non haploid genotype" $ 
            \s c-> 
                (s :: Int) > 0 && (c :: Int) > 2 QC.==> 
                    ((isHaploid $ mkDummyDiploidGenotype s) || (isHaploid $ mkDummyPolyploidGenotype s c)) == False,
        QC.testProperty "isDiploid should return True for diploid genotype" $ 
            \s -> 
                (s :: Int) > 1 QC.==> isDiploid $ mkDummyDiploidGenotype s,         
        QC.testProperty "isDiploid should return False for non diploid genotype" $ 
            \s c-> 
                (s :: Int) > 0 && (c :: Int) > 2 QC.==> 
                    ((isDiploid $ mkDummyHaploidGenotype s) || (isDiploid $ mkDummyPolyploidGenotype s c)) == False,
        QC.testProperty "isPolyploid should return True for polyploid genotype" $ 
            \s c-> 
                (s :: Int) > 0 && (c :: Int) > 2 QC.==> isPolyploid $ mkDummyPolyploidGenotype s c,         
        QC.testProperty "isPolyploid should return False for non polyploid genotype" $ 
            \s -> 
                (s :: Int) > 1 QC.==> 
                    ((isPolyploid $ mkDummyHaploidGenotype s) || (isPolyploid $ mkDummyDiploidGenotype s)) == False,
        QC.testProperty "Verify generateGamete produces a gamete with the correct number of chromosomes" $
            \s seed ->
                (s :: Int) > 0 QC.==> 
                    (length $ generateGamete (mkRandom seed) (mkDummyDiploidGenotype s)) == s,
        QC.testProperty "Verify generateGamete only works with diploid genotype " $
            \s c seed ->
                 (s :: Int) > 0 && (c :: Int) > 2 QC.==> 
                    let h = length $ generateGamete (mkRandom seed) (mkDummyHaploidGenotype s)
                        p = length $ generateGamete (mkRandom seed) (mkDummyPolyploidGenotype s c)
                        in (h + p) == 0,
        QC.testProperty "Verify that all genes returned in generateGamete are in the original genotype" $
            \seed ->
                all (\g -> elem g (unravelGenotype mkTestGenotype)) (unravelChromosomes $ generateGamete (mkRandom seed) mkTestGenotype)
    ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [
        testCase "Verify homologous returns False for empty sets" $ 
            homologous [] @?= False,
        testCase "Verify nullizygous returns True for empty sets" $ 
            nullizygous [] @?= True, 
        testCase "Verify hemizygous returns False for empty sets" $ 
            hemizygous [] @?= False, 
        testCase "Verify hemizygous returns True for just one chromosome" $ 
            hemizygous [mkDummyGene 1] @?= True
    ]

mkDummyGene :: Int -> Gene
mkDummyGene i = Gene { geneSymbol = "dummy" ++ show i, geneDescription = "", geneDominance = i, geneBlob = NullBlob }

mkDummyChromosome :: Int -> Chromosome
mkDummyChromosome l = Chromosome {
        chromosomeGenes = map (\i -> (ILocus i, mkDummyGene i)) [1..l]        
    }

mkDummyHaploidGenotype :: Int -> Genotype
mkDummyHaploidGenotype s = Genotype {
        genotypeChromosomes = map (\i -> [mkDummyChromosome 3]) [1..s]
    }

mkDummyDiploidGenotype :: Int -> Genotype
mkDummyDiploidGenotype s = Genotype {
        genotypeChromosomes = map (\i -> [mkDummyChromosome 3, mkDummyChromosome 3]) [1..s]
    }

mkDummyPolyploidGenotype :: Int -> Int -> Genotype
mkDummyPolyploidGenotype s c = Genotype {
        genotypeChromosomes = map (\i -> replicate c (mkDummyChromosome 3)) [1..s]
    }


mkTestGenotype :: Genotype        
mkTestGenotype = Genotype {
        genotypeChromosomes = [
            -- Set 1
            [
                Chromosome {
                    chromosomeGenes = [
                        (ILocus 1, mkDummyGene 1),
                        (ILocus 2, mkDummyGene 2),
                        (ILocus 1, mkDummyGene 3)
                    ]
                },
                Chromosome {
                    chromosomeGenes = [
                        (ILocus 1, mkDummyGene 4),
                        (ILocus 2, mkDummyGene 5),
                        (ILocus 3, mkDummyGene 6)
                    ]
                }
            ], 
            -- Set 2
            [
                Chromosome {
                    chromosomeGenes = [
                        (ILocus 4, mkDummyGene 7),
                        (ILocus 5, mkDummyGene 8),
                        (ILocus 6, mkDummyGene 9)
                    ]
                },
                Chromosome {
                    chromosomeGenes = [
                        (ILocus 4, mkDummyGene 7),
                        (ILocus 5, mkDummyGene 8),
                        (ILocus 6, mkDummyGene 9)
                    ]
                }
            ],
            -- Set 3
            [
                Chromosome {
                    chromosomeGenes = []
                },
                Chromosome {
                    chromosomeGenes = [
                        (ILocus 7, mkDummyGene 10),
                        (ILocus 8, mkDummyGene 11),
                        (ILocus 9, mkDummyGene 12)
                    ]
                }
            ]
        ]
    } 

unravelChromosome :: Chromosome -> [Gene]
unravelChromosome = map (\(l, g) -> g ) . chromosomeGenes

unravelChromosomes :: [Chromosome] -> [Gene]
unravelChromosomes = concat . map (\c -> unravelChromosome c)

unravelGenotype :: Genotype -> [Gene]
unravelGenotype = concat . map (unravelChromosomes) . genotypeChromosomes

mkLocusList :: Int -> [Locus] 
mkLocusList n = map ILocus [1..n]
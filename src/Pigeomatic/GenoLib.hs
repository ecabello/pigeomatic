-- | Very simplified implementation of Mendelian genetics
module Pigeomatic.GenoLib (
    Blob(..),
    Gene(..),
    Locus(..),
    Chromosome(..),
    Genotype(..),
    LocusHandler,
    Trait(..),
    Phenotype(..),
    loci,
    locusIn,
    ploidyNumber,
    chromosomeCount,
    alleleGenes,
    homologous,
    nullizygous,
    hemizygous,
    homozygous,
    heterozygous,
    isHaploid,
    isDiploid,
    isPolyploid,        
    generateGamete,
    combineGametes,
    computePhenotype
) where

import qualified Data.ByteString as BS
import Data.List (find)
import Pigeomatic.Utils

-- | 'Blob' represents arbitrary data that can be attached to other entities
data Blob = 
    NullBlob | 
    StringBlob String |
    NumericBlob Double |
    ByteStringBlob BS.ByteString
    deriving (Eq, Show, Read)

-- | 'Gene' This is the basic physical and functional unit of heredity.
-- Genes are passed from parents to offspring and contain the information needed to specify traits. 
-- They are arranged, one after another, on structures called chromosomes.
-- Genes have an identifying symbol and a dominance level which determine how traits are expressed.
data Gene = Gene {
    geneSymbol :: String,
    geneDescription :: String,
    geneDominance :: Int,
    geneBlob :: Blob
} deriving (Show, Read)

-- | 'Eq' Two genes are the same if they have the same symbol and dominance level
instance Eq Gene where  
    x == y = geneSymbol x == geneSymbol y 
        && geneDominance x == geneDominance y    

-- | 'Locus' is the specific physical location of a gene on a chromosome
data Locus = 
    SLocus String |
    ILocus Int
    deriving (Eq, Show, Read)

-- | 'Chromosome' is an organized package of DNA found in the nucleus of the cell.
-- At a basic level they are a collection of Genes. Each Gene resides at a locus
data Chromosome = Chromosome {
    chromosomeGenes :: [(Locus, Gene)]
} deriving (Show, Read)

-- | 'loci' returns all the Gene locations present in the specified chromosome
loci :: Chromosome -> [Locus]
loci  = map locus . chromosomeGenes 
    where locus (l, _) = l

-- | 'locusIn' determines if the specified Locus is present on the chromosome
locusIn :: Locus -> Chromosome -> Bool
locusIn l = elem l . loci

-- | 'Genotype' is an organism's collection of genes.
-- These genes are organized in a collection of homologous sets of chromosomes
data Genotype = Genotype {
    genotypeChromosomes :: [[Chromosome]]
} deriving (Show, Read)

-- | 'ploidyNumber' determines the number of sets of chromosomes in the specified genotype
ploidyNumber :: Genotype -> Int
ploidyNumber = length . genotypeChromosomes

-- | 'chromosomeCount' determines the number of chromosomes in the specified genotype
chromosomeCount :: Genotype -> Int
chromosomeCount = length . concat . genotypeChromosomes

-- | 'alleleGenes' given a locus and a chromosome set it returns the corresponding genes
alleleGenes :: Locus -> [Chromosome] -> [Gene]
alleleGenes l = map snd . filter isLocus . concat . map chromosomeGenes
    where isLocus (locus, _) = l == locus

-- | 'homologous' determines if the chromosomes in the specified set have the same loci
homologous :: [Chromosome] -> Bool
homologous xs@(x1:x2:_) = all (\c -> loci x1 == loci c) xs
homologous _ = False;

-- | 'nullizygous' determines if the specified gene set is empty
nullizygous :: [Gene] -> Bool
nullizygous [] = True
nullizygous _ = False

-- | 'hemizygous' determines if there is only one copy of the gene
hemizygous :: [Gene] -> Bool
hemizygous [x] = True
hemizygous _ = False

-- | 'homozygous' determines if all genes in the set are the same
homozygous :: [Gene] -> Bool
homozygous xs@(x1:x2:_) = all (==x1) xs
homozygous _ = False;

-- | 'heterozygous' determines if at least some genes in the set are different
heterozygous :: [Gene] -> Bool
heterozygous xs@(x1:x2:_) = any (/=x1) xs
heterozygous _ = False;

-- | 'isNploid' utility function to check ploid condition
isNploid :: ([Chromosome] -> Bool) -> Genotype -> Bool
isNploid f = all f . genotypeChromosomes

-- | 'isHaploid' determines if the specified genotype belongs to haploid organism
isHaploid :: Genotype -> Bool
isHaploid = isNploid (\cs -> length cs == 1 )

-- | 'isDiploid' dtermines if the specified genotype belongs to diploid organism
isDiploid :: Genotype -> Bool
isDiploid = isNploid (\cs -> length cs == 2 )

-- | 'isPolyploid' dtermines if the specified genotype belongs to polyploid organism
isPolyploid :: Genotype -> Bool
isPolyploid = isNploid (\cs -> length cs > 2 )

-- | 'generateGamete' given a random number generator and a diploid genotype it generates a gamete.
-- Gametes are reproductive cells having the haploid number of chromosomes, especially a mature sperm or egg 
-- capable of fusing with a gamete of the opposite sex to produce a zygote. 
generateGamete :: Random -> Genotype -> [Chromosome]
generateGamete rnd g | isDiploid g = map recombine $ genotypeChromosomes g
                     | otherwise = []
                     where recombine cs | homologous cs = Chromosome { chromosomeGenes=(map (\(i, l) -> (l, (alleleGenes l cs) !! i)) (zip rs (loci $ head cs))) }
                                        | otherwise = cs !! (rs !! 15)
                                        where rs = reverse $ toBin64 $ fst $ nextRandom rnd

-- | 'combineGametes' given two gametes, it fuses them to produce a zygote 
combineGametes :: [Chromosome] -> [Chromosome] -> Genotype
combineGametes g1 g2 = Genotype { genotypeChromosomes=( map (\(c1, c2) -> [c1,c2]) $ zip g1 g2 ) }

-- | 'Trait' represents a phenotype trait. 
data Trait = Trait {
    traitName :: String,
    traitDescription :: String,
    traitBlob :: Blob
} deriving (Eq, Show, Read)

-- | 'Phenotype' is an organism's observable traits. Their expression is determined by a genotype.
data Phenotype = Phenotype {
    phenotypeTraits :: [Trait]
} deriving (Show, Read)


-- | 'LocusHandler' is a function that processes allele genes and computes the resulting trait(s)
type LocusHandler = (Locus -> [Gene] -> [Trait])

-- | 'computePhenotype' is a function that takes a set of handlers and a genotype and computes the phenotype
computePhenotype :: [(Locus, LocusHandler)] -> Genotype -> Phenotype
computePhenotype hs = mkPhenotype . removeDups . concat . (map computeTraits) . genotypeChromosomes
    where mkPhenotype ts = Phenotype {phenotypeTraits=(ts)}         
          removeDups (x:xs) = x : removeDups (filter (/= x) xs)
          removeDups [] = []
          getLoci = removeDups . concat . (map loci)
          computeTraits cs = concat $ map computeLocusTraits $ getLoci cs
              where computeLocusTraits l = case (find (\(x,_) -> x == l) hs) of
                                               Nothing -> []
                                               Just (_,h) -> h l (alleleGenes l cs)
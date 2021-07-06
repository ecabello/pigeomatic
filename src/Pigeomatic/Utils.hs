-- | Utility functions
module Pigeomatic.Utils(
    Random,
    mkRandom,
    nextRandom,
    randomInts,
    skipRandom,
    pickRandom,
    toBin,
    toBinBase,
    toBin32,
    toBin64,
    hashSHA256,
    toIso8601,
    fromIso8601,
    merge,
    allEqual
) where

import Data.List (sort, sortBy)
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format (formatTime, parseTimeM, defaultTimeLocale)
import Data.Time.Format.ISO8601
import Crypto.Hash
import Crypto.Hash.Algorithms
import qualified Data.ByteString.UTF8 as BS
import Data.Word (Word64)
import System.Random.Mersenne.Pure64

-- | 'toBin' builds a binary representation of the passed Int
-- as a list of 0s and 1s
toBin :: Int -> [Int]
toBin 0 = [0]
toBin n = reverse (proc $ abs n)
    where proc 0 = []
          proc n = let (q,r) = n `divMod` 2 in r : proc q

-- | 'toBinBase' a version of toBin that allows you to
-- specify  the minimum number of elements in the resulting
-- list of 0s and 1s
toBinBase :: Int -> Int -> [Int]
toBinBase n base | base > 0 = padding ++ bin
                 | otherwise = []
    where bin = toBin n
          extra = base - length bin
          padding = if extra > 0 then replicate extra 0
                                 else []

-- | 'toBin32' is a convenient function equivalent to:
-- toBinBase n 32
toBin32 :: Int -> [Int]
toBin32 n = toBinBase n 32

-- | 'toBin64' is a convenient function equivalent to:
-- toBinBase n 64
toBin64 :: Int -> [Int]
toBin64 n = toBinBase n 64

-- | Encapsulates a pure random number generator
type Random = (Int, PureMT)

-- | 'mkRandom' builds a pure random number generator from the given seed
mkRandom :: Word64 -> Random
mkRandom seed = randomInt $ pureMT seed

-- | 'nextRandom' returns the next random in the sequence
-- You can use fst rnd to extract the random Int
nextRandom :: Random -> Random
nextRandom = randomInt . snd

-- | 'randomInts' returns a list of random Ints from the sequence
randomInts :: Random -> Int -> [Int]
randomInts rnd n | n == 0 = []
                 | otherwise = map (\i -> fst $ skipRandom i rnd) [1..n]

-- | 'skipRandom' skips the specified number of slots in the random sequence
skipRandom :: Int -> Random -> Random
skipRandom n r = foldl (\r _ -> nextRandom r) r [1..n]

-- | 'pickRandom' picks randomly from a list of optional values
pickRandom :: Random -> [a] -> a
pickRandom rnd xs = snd $ head $ sortBy cmp $ zip rs xs
    where rs = randomInts rnd $ length xs
          cmp (i, _) (j, _) | i<j = LT
                            | i>j = GT
                            | otherwise = EQ

-- | 'hashSHA256' generates the SHA256 hash for the specified string 
hashSHA256 :: String -> String
hashSHA256 s = let h = hash $ BS.fromString s :: Digest SHA256
    in show h

-- | 'toIso8601' converts the passed UTCTime to its ISO8601 representation
toIso8601 :: UTCTime -> String
toIso8601 = iso8601Show
--toIso8601 = formatTime defaultTimeLocale "%FT%T%QZ"

-- | 'fromIso8601' tries to parse a string in ISO8601 format to UTCTime
fromIso8601 :: String -> Maybe UTCTime
fromIso8601 = iso8601ParseM 
--fromIso8601 = parseTimeM True defaultTimeLocale "%FT%T%QZ"

-- | 'merge' merges two lists into one 
merge :: [a] -> [a] -> [a]
merge [] ys = ys
merge (x:xs) ys = x:merge ys xs

-- | 'allEqual' checks if all elements in a list are equal
allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (== x) xs

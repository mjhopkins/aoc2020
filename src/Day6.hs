module Day6 where

import           Control.Monad    (join)
import           Data.List.Extra  (wordsBy)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE input #-}
input :: String
input = unsafePerformIO . readFile $ "data/day6.txt"

inputGroups :: [Set Char]
inputGroups = parseAnyGroups input

inputAllGroups :: [Set Char]
inputAllGroups = parseAllGroups input

inputCounts :: Int
inputCounts = sumOfCounts input

inputAllCounts :: Int
inputAllCounts = sumOfAllCounts input

parseAnyGroups :: String -> [Set Char]
parseAnyGroups
  = map (Set.fromList . join)
  . parseGroups

parseAllGroups :: String -> [Set Char]
parseAllGroups
  = map (foldl1 Set.intersection . map Set.fromList)
  . parseGroups

parseGroups :: String -> [[String]]
parseGroups
  = map (map (map checkValid))
  . wordsBy (== "")
  . lines
  where
    checkValid c = if c `elem` ['a'..'z'] then c else error "invalid char"

sumOfCounts :: String -> Int
sumOfCounts = sum . map Set.size . parseAnyGroups

sumOfAllCounts :: String -> Int
sumOfAllCounts = sum . map Set.size . parseAllGroups

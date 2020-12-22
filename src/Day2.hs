module Day2 where

import Data.Bool
import Text.Regex.TDFA
import System.IO.Unsafe

countValidLines :: String -> Int
countValidLines = countVL validate

countValidLines' :: String -> Int
countValidLines' = countVL validate'

countVL :: (Policy -> String -> Bool) -> String -> Int
countVL f
  = sum 
  . map (bool 0 1) 
  . map (uncurry f) 
  . map parseLine 
  . lines

data Policy = Policy Int Int Char
  deriving (Eq, Show)

parseLine :: String -> (Policy, String) 
parseLine s = 
  let 
    match, pre, post :: String
    (match, pre, post, submatches) = s =~ lineRegex
  in case submatches of
    [lo, hi, [ch], str] -> (Policy (read lo) (read hi) ch, str)
    other               -> 
        error $ "Bad line: " ++ s 
         ++ ", submatches: " ++ show submatches
         ++ ", match: " ++ match
         ++ ", pre: " ++ pre
         ++ ", post: " ++ post

lineRegex :: String
lineRegex = "([0-9]+)-([0-9]+) ([a-zA-Z]): (.*)"

validate :: Policy -> String -> Bool
validate (Policy lo hi ch) s
  = let count = length . filter (==ch) $ s
    in lo <= count && count <= hi

validate' :: Policy -> String -> Bool
validate' (Policy lo hi ch) s
  =  (s !! (lo-1) == ch)
  /= (s !! (hi-1) == ch)

{-# NOINLINE input #-}
input :: String
input = unsafePerformIO . readFile $ "data/day2.txt"    

parsed :: [(Policy, String)]
parsed = map parseLine . lines $ input
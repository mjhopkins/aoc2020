module Day9 where

import           System.IO.Unsafe
import Text.Parsec (Parsec)
import qualified Text.Parsec as P
import Data.Foldable

-------------------------------------------------------------------------------
-- Answer
-------------------------------------------------------------------------------

findNotSum preambleLength is
  = let (preamble, input) = splitAt preambleLength is
    in undefined
      -- sca
      -- foldl' (\acc i -> if valid i acc then i : acc else _op) preamble input

valid :: a -> [a] -> Bool
valid = error "not implemented"

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

type Parser = Parsec String ()

parse :: String -> [Integer]
parse = either (error . show) id . traverse (P.parse integer "") . lines

integer :: Parser Integer
integer = read <$> P.many1 P.digit

-------------------------------------------------------------------------------
-- Input data
-------------------------------------------------------------------------------

{-# ANN input "HLint: ignore Missing NOINLINE pragma" #-}
input :: String
input = unsafePerformIO . readFile $ "data/day8.txt"

inputData :: [Integer]
inputData = parse input
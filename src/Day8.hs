-- {-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Day8 where

import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import qualified Data.List.Extra     as L
import           Data.Maybe          (fromMaybe, isJust)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Traversable
import           Safe                (atMay)
import           System.IO.Unsafe    (unsafePerformIO)
import           Text.Parsec         (Parsec)
import qualified Text.Parsec         as P

data Instruction = Instruction Operation Int
  deriving Eq

instance Show Instruction where
  show (Instruction op n) =  L.lower (show op) ++ " " ++ show n

data Operation = Acc | Jmp | Nop
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

type Parser = Parsec String ()

p :: Parser Instruction
p = do
  op <- operation
  P.char ' '
  sign <- 1 <$ P.char '+' <|> (-1) <$ P.char '-'
  n  <- int
  return $ Instruction op (sign * n)

parse :: String -> [Instruction]
parse = either (error . show) id . traverse (P.parse p "") . lines

int :: Parser Int
int = read <$> P.many1 P.digit

operation :: Parser Operation
operation = (Acc <$ P.string "acc") <|> Jmp <$ P.string "jmp" <|> Nop <$ P.string "nop"

-------------------------------------------------------------------------------
-- Answer
-------------------------------------------------------------------------------

accValueOnLoop :: [Instruction] -> Maybe Int
accValueOnLoop
  = run 0 0 Set.empty
  where
    run :: Int -> Int -> Set Int -> [Instruction] -> Maybe Int
    run i acc visited instrs =
      if Set.member i visited then Just acc
      else
        case instrs !!? i of
          Just (Instruction Nop _) -> run (i+1) acc       (Set.insert i visited) instrs
          Just (Instruction Acc n) -> run (i+1) (acc + n) (Set.insert i visited) instrs
          Just (Instruction Jmp n) -> run (i+n) acc       (Set.insert i visited) instrs
          Nothing -> Nothing

(!!?) :: [a] -> Int -> Maybe a
(!!?) = atMay

accValueAfterFix :: [Instruction] -> Int
accValueAfterFix instrs
  = fromMaybe (error "no solution")
  $ join
  $ find isJust
  $ map (run 0 0 Set.empty . alter)
  $ [1.. length instrs]
  where
    alter :: Int -> [Instruction]
    alter i = case instrs !! i of
      Instruction Nop n -> subst i (Instruction Jmp n) instrs
      Instruction Jmp n -> subst i (Instruction Nop n) instrs
      Instruction Acc n -> subst i (Instruction Acc n) instrs

    -- run :: [Instruction] -> Maybe Int
    run i acc visited instrs =
      if Set.member i visited then Nothing
      else case instrs !!? i of
        Just (Instruction Nop _) -> run (i+1) acc     (Set.insert i visited) instrs
        Just (Instruction Jmp n) -> run (i+n) acc     (Set.insert i visited) instrs
        Just (Instruction Acc n) -> run (i+1) (acc+n) (Set.insert i visited) instrs
        Nothing                -> Just acc

subst :: Int -> a -> [a] -> [a]
subst _ _  []     = error "out of range"
subst 0 a' (_:as) = a' : as
subst n a' (a:as) = a : subst (n - 1) a' as



-------------------------------------------------------------------------------
-- Input data
-------------------------------------------------------------------------------

{-# ANN input "HLint: ignore Missing NOINLINE pragma" #-}
input :: String
input = unsafePerformIO . readFile $ "data/day8.txt"

inputInstructions :: [Instruction]
inputInstructions = parse input

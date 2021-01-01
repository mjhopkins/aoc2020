{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# OPTIONS_GHC -ddump-deriv #-}

module Day4 where

import           Control.Monad
import           Data.Bifunctor
import           Data.Either
import           Data.List.Extra
import Control.Applicative
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Maybe
import           GHC.Read         (Read (readPrec), choose, parens)
import           System.IO.Unsafe (unsafePerformIO)
import           Text.Read
import           Text.Regex.TDFA

newtype PassportData = PassportData (Map Field String)
  deriving (Eq, Show)

instance Semigroup PassportData where
  PassportData m1 <> PassportData m2
    = PassportData
    $ Map.unionWithKey err  m1 m2
    where
      err k v1 v2
        = error $ "Clashing values for field" ++ code k ++ ": " ++ v1 ++ ", " ++ v2

instance Monoid PassportData where
  mempty = PassportData Map.empty

data TypedPassportData
  = TypedPassportData
  { birthYear      :: Int
  , issueYear      :: Int
  , expirationYear :: Int
  , height         :: Either Int Int
  , hairColor      :: String
  , eyeColor       :: String
  , passportID     :: Int
  }
  deriving (Eq, Show)

validate :: PassportData -> Either String TypedPassportData
validate = validate' <=< checkMandatoryFieldsPresent

checkMandatoryFieldsPresent :: PassportData -> Either String PassportData
checkMandatoryFieldsPresent pd = if hasRequiredKeys pd then Right pd else Left "Missing fields"

-- assumes all mandatory fields are present
validate' :: PassportData -> Either String TypedPassportData
validate' (PassportData m)
  = TypedPassportData
   <$> validBirthYear      (get BirthYear)
   <*> validIssueYear      (get IssueYear)
   <*> validExpirationYear (get ExpirationYear)
   <*> validHeight         (get Height)
   <*> validHairColor      (get HairColor)
   <*> validEyeColor       (get EyeColor)
   <*> validPassportID     (get PassportID)
  where
    get k = fromMaybe (error $ "Missing field " ++ code k) $ Map.lookup k m

validBirthYear :: String -> Either String Int
validBirthYear = yearRange 1920 2002 "Birth year"

validIssueYear :: String -> Either String Int
validIssueYear = yearRange 2010 2020 "Issue year"

validExpirationYear :: String -> Either String Int
validExpirationYear = yearRange 2020 2030 "Expiration year"

yearRange :: (Ord c, Read c) => c -> c -> [Char] -> String -> Either [Char] c
yearRange lo hi s
  = (\i -> if lo <= i && i <= hi then Right i else Left (s ++ " out of range"))
  <=< readEither

validHeight :: String -> Either String (Either Int Int)
validHeight s = do
  mcm <-  tryMatch "\\`([0-9]{3})cm\\'" s
  min <-  tryMatch "\\`([0-9]{2})in\\'" s
  x <- maybe (Right Nothing) (fmap Just . checkCmRange) mcm
  y <- maybe (Right Nothing) (fmap Just . checkInRange) min
  let z = x <|> y 
  let w = maybe (Left "Height must be given either in cm or in") Right z
  w

  where
    checkCmRange i 
      = if 150 <= i && i <= 193 
        then Right (Left i) 
        else Left "cm height out of range"
    checkInRange i 
      = if 59 <= i && i <= 76 
        then Right (Right i) 
        else Left "in height out of range"

tryMatch :: String -> String -> Either String (Maybe Int)
tryMatch r s
  = let
      match, pre, post :: String
      submatches :: [String]
      (match, pre, post, submatches) = s =~ r
    in
      case submatches of
        [m] -> maybe (Left $ "Height could not be parsed as an int: [" ++ m ++ "]") (Right . Just) $ readMaybe m
        []  -> return Nothing
        _   -> error "wrong number of submatches"

validHairColor :: String -> Either String String
validHairColor s = if s =~ r then Right s else Left "Invalid hair colour"
  where
    r = "\\`#[0-9a-f]{6}\\'"

validEyeColor :: String -> Either String String
validEyeColor "amb" = Right "amb"
validEyeColor "blu" = Right "blu"
validEyeColor "brn" = Right "brn"
validEyeColor "gry" = Right "gry"
validEyeColor "grn" = Right "grn"
validEyeColor "hzl" = Right "hzl"
validEyeColor "oth" = Right "oth"
validEyeColor _     = Left "Invalid eye color"

validPassportID :: String -> Either String Int
validPassportID s = if s =~ "\\`[0-9]{9}\\'" then Right (read s) else Left "Invalid passport id"

data Validity = Valid | Invalid
  deriving (Eq, Show)

hasRequiredKeys :: PassportData -> Bool
hasRequiredKeys (PassportData m) = all (`elem` keys) required
  where
    keys = Map.keys m
    required =
      [ BirthYear
      , IssueYear
      , ExpirationYear
      , Height
      , HairColor
      , EyeColor
      , PassportID
      ]

countValid :: [PassportData] -> Int
countValid = sum . map (\pd -> if hasRequiredKeys pd then 1 else 0)

countValidated :: [PassportData] -> Int
countValidated = sum . map (either (const 0) (const 1) . validate)

data Field
  = BirthYear
  | IssueYear
  | ExpirationYear
  | Height
  | HairColor
  | EyeColor
  | PassportID
  | CountryID
  deriving (Eq, Ord, Show)

parse :: String -> Either String [PassportData]
parse = traverse parseRecord . records . lines

parseUnsafe :: String -> [PassportData]
parseUnsafe = either error id . parse

parseRecord :: String -> Either String PassportData
parseRecord
  = fmap (foldMap (\(k,v) -> PassportData $ Map.singleton k v))
  . traverse parseEntry
  . words

parseEntry :: String -> Either String (Field, String)
parseEntry
  = parseKV
  . split (== ':')

parseKV :: [String] -> Either String (Field, String)
parseKV [k,v] = Right (read k, v)
parseKV other = Left $ "Expected key-val pair but got: " ++ show other

records :: [String] -> [String]
records = map unwords . wordsBy (== "")

-- records = go [] []
--   where
--     go acc curr []      = acc
--     go acc curr ("":ss) = undefined

-- instance Show Field where
--   show BirthYear      = "byr"
--   show IssueYear      = "iyr"
--   show ExpirationYear = "eyr"
--   show Height         = "hgt"
--   show HairColor      = "hcl"
--   show EyeColor       = "ecl"
--   show PassportID     = "pid"
--   show CountryID      = "cid"

code :: Field -> String
code BirthYear      = "byr"
code IssueYear      = "iyr"
code ExpirationYear = "eyr"
code Height         = "hgt"
code HairColor      = "hcl"
code EyeColor       = "ecl"
code PassportID     = "pid"
code CountryID      = "cid"

instance Read Field where
  readPrec = parens . choose $
    [ ("byr", return BirthYear)
    , ("iyr", return IssueYear)
    , ("eyr", return ExpirationYear)
    , ("hgt", return Height)
    , ("hcl", return HairColor)
    , ("ecl", return EyeColor)
    , ("pid", return PassportID)
    , ("cid", return CountryID)
    ]

-- instance Read Field where
--   readsPrec d s = case (read s :: String) of
--     "byr" -> return BirthYear
--     "iyr" -> return IssueYear
--     "eyr" -> return ExpirationYear
--     "hgt" -> return Height
--     "hcl" -> return HairColor
--     "ecl" -> return EyeColor
--     "pid" -> return PassportID
--     "cid" -> return CountryID
--     other -> fail ""

inputData :: [PassportData]
inputData = parseUnsafe input

{-# NOINLINE input #-}
input :: String
input = unsafePerformIO . readFile $ "data/day4.txt"

sample :: String
sample = unlines
  [ "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
  , "byr:1937 iyr:2017 cid:147 hgt:183cm"
  , ""
  , "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
  , "hcl:#cfa07d byr:1929"
  , ""
  , "hcl:#ae17e1 iyr:2013"
  , "eyr:2024"
  , "ecl:brn pid:760753108 byr:1931"
  , "hgt:179cm"
  , ""
  , "hcl:#cfa07d eyr:2025 pid:166559648"
  , "iyr:2011 ecl:brn hgt:59in"
  ]

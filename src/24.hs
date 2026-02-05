module MyParsers where

import Control.Applicative
import Data.Bool (bool)
import Data.Char (digitToInt)
import Data.Foldable
import Data.Maybe (catMaybes, isJust)
import Text.Trifecta

--------------------------------------------------------------------------------
-- SemVer
--------------------------------------------------------------------------------

data NumberOrString = NOSS String | NOSI Integer deriving (Eq, Show)

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Eq, Show)

instance Ord SemVer where
  compare (SemVer ma1 mi1 pa1 _ _) (SemVer ma2 mi2 pa2 _ _) = compare [ma1, mi1, pa1] [ma2, mi2, pa2]

parseNumOrStr :: Parser NumberOrString
parseNumOrStr = (NOSI <$> try (integer <* notFollowedBy alphaNum)) <|> (NOSS <$> some alphaNum)

parseRelease :: Parser Release
parseRelease = do
  _ <- char '-'
  sepBy1 parseNumOrStr $ char '.'

parseMetadata :: Parser Metadata
parseMetadata = do
  _ <- char '+'
  sepBy1 parseNumOrStr $ char '.'

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  _ <- char '.'
  minor <- integer
  _ <- char '.'
  patch <- integer
  rel <- try parseRelease <|> return []
  meta <- try parseMetadata <|> return []
  eof
  return $ SemVer major minor patch rel meta

--------------------------------------------------------------------------------
-- Integers
--------------------------------------------------------------------------------

parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

base10Integer :: Parser Integer
base10Integer = do
  str <- some parseDigit
  return $ foldl' go 0 str
  where
    go acc x = 10 * acc + toInteger (digitToInt x)

base10Integer' :: Parser Integer
base10Integer' = do
  sign <- optional $ char '-'
  int <- base10Integer
  return $ if isJust sign then negate int else int

--------------------------------------------------------------------------------
-- Phone Numbers
--------------------------------------------------------------------------------

type NumberingPlanArea = Int

type Exchange = Int

type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)

parseTrunk :: Parser String
parseTrunk = do
  plus <- optional $ char '+'
  trunk <- digit
  sep <- oneOf "- "
  return $ catMaybes [plus, Just trunk, Just sep]

parseNPA :: Parser NumberingPlanArea
parseNPA = read <$> count 3 parseDigit

parseEx :: Parser Exchange
parseEx = read <$> count 3 parseDigit

parseLN :: Parser LineNumber
parseLN = read <$> count 4 parseDigit

parsePhoneDashOnly :: Parser PhoneNumber
parsePhoneDashOnly = do
  npa <- parseNPA
  _ <- char '-'
  ex <- parseEx
  _ <- char '-'
  ln <- parseLN
  eof
  return $ PhoneNumber npa ex ln

parsePhoneComplex :: Parser PhoneNumber
parsePhoneComplex = do
  mnpa <- optional $ between (char '(') (char ')') parseNPA
  case mnpa of
    Just npa -> do
      _ <- char ' '
      ex <- parseEx
      _ <- oneOf "- "
      ln <- parseLN
      eof
      return $ PhoneNumber npa ex ln
    Nothing -> do
      npa <- parseNPA
      lastSeparator <- optional $ bool "- " "-" . (== '-') <$> oneOf "- "
      case lastSeparator of
        Nothing -> do
          ex <- parseEx
          ln <- parseLN
          eof
          return $ PhoneNumber npa ex ln
        Just separators -> do
          ex <- parseEx
          _ <- oneOf separators
          ln <- parseLN
          eof
          return $ PhoneNumber npa ex ln

parsePhone :: Parser PhoneNumber
parsePhone = do
  useDash <- (== '-') . last <$> option " " (try parseTrunk)
  if useDash then parsePhoneDashOnly else parsePhoneComplex

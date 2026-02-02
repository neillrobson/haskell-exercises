module MyParsers where

import Control.Applicative
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

parseNumOrStr :: Parser NumberOrString
parseNumOrStr = (NOSI <$> integer) <|> (NOSS <$> some letter)

parseRelease :: Parser Release
parseRelease = do
  _ <- char '-'
  sepBy1 parseNumOrStr $ char '.'

parseMetadata :: Parser Metadata
parseMetadata = do
  _ <- char '+'
  sepBy1 parseNumOrStr $ char '.'

-- TODO: Fails on "1.0.0-beta+oof.sha.41af286"

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

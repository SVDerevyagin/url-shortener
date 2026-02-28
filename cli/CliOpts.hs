{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Parses command line options

module CliOpts
  ( Options(..)
  , options
  ) where

import Options.Applicative
import Data.Time
import qualified Data.Text as T
import           Data.Text (Text)

-- | the datatype representing all available cli options
data Options
  = Shorten   { ocOriginalURL    :: Text
              , ocShortURL       :: Maybe Text
              , ocExpirationDate :: Maybe Day
              } -- ^ shorten command: creates a new short link in the database
  | Open      { ooShortURL :: Text
              } -- ^ open command: returns the original link
  | Analytics { oaShortURL :: Text
              } -- ^ analytics command: returns the amount of clicks
  deriving (Show)


-- | parses the options and creates help messages for the app
options :: IO Options
options
  = execParser
  $ info (optionParser <**> helper)
  $ progDesc "sends a command to the database"

-- | main parser, combines parsers for each command
optionParser :: Parser Options
optionParser
   = hsubparser
   $ command "shorten"   ( info shortenOptionsParser
                         $ progDesc "shortens a link"
                         )
  <> command "open"      ( info openOptionsParser
                         $ progDesc "opens a short link"
                         )
  <> command "analytics" ( info analyticsOptionsParser
                         $ progDesc "returns amount of clicks on this short link"
                         )

-- | parser for the =shorten= command
shortenOptionsParser :: Parser Options
shortenOptionsParser
   =  Shorten
  <$> originalURLParser
  <*> shortURLParser
  <*> expDateParser

-- | parser for the =open= command
openOptionsParser :: Parser Options
openOptionsParser
   =  Open
  <$> shortLinkParser "open"

-- | parser for the =analytics= command
analyticsOptionsParser :: Parser Options
analyticsOptionsParser
   =  Analytics
  <$> shortLinkParser "analyze"

-- | parser for the =original URL= option (=shorten= command)
originalURLParser :: Parser Text
originalURLParser
   = strOption
   $ metavar "<ORIGINAL URL>"
  <> help "the URL you want to shorten"

-- | parser for the optional =short URL= option (=shorten= command)
shortURLParser :: Parser (Maybe Text)
shortURLParser
   = optional $ strOption
   $ long "short-url"
  <> short 's'
  <> metavar "<SHORT URL>"
  <> help "(optional) custom short URL"

-- | parser for the optional =expiration date= option (=shorten= command)
expDateParser :: Parser (Maybe Day)
expDateParser
   = optional $ option auto
   $ long "expiration-date"
  <> short 'd'
  <> metavar "YYYY-MM-DD"
  <> help "(optional) the date when the short link should expire"

-- | parser for the =short URL= option (=open= and =analytics= commands)
shortLinkParser :: Text -> Parser Text
shortLinkParser verb
   = strArgument
   $ metavar "<SHORT URL>"
  <> help ("the URL you want to " ++ T.unpack verb)

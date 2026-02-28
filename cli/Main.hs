{-# LANGUAGE OverloadedStrings #-}

-- | Command Line Interface
-- sends a command to the database
--
-- Available commands:
--   shorten      shortens a link
--   open         opens a short link
--   analytics    returns amount of clicks on this short link

module Main (main) where

import Control.Monad (void)
import Control.Monad.State (liftIO)
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import CliOpts
import USh.DB
import USh.Utils

-- | main
main :: IO ()
main = do
  opts <- options
  void $ runMain True $ main' opts

-- | sends a command to the database depending on the command given to the cli
main' :: Options -> MainError ()
main' (Shorten oURL sURL eDate) = do
  sl <- createShortURL oURL sURL (dayToUtc <$> eDate)
  liftIO $ T.putStrLn $ "Your short link is «" <> sShortURL sl <> "»"

main' (Open sURL) = do
  ol <- openShortURL sURL
  liftIO $ T.putStrLn $ "Your original link is «" <> ol <> "»"

main' (Analytics sURL) = do
  n <- getAnalytics sURL
  liftIO $ putStrLn $ "This link was clicked " ++ show n ++ " times"

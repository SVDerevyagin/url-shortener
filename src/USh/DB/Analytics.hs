{-# LANGUAGE OverloadedStrings #-}

-- | Encapsulates getting the usage analytics

module USh.DB.Analytics
  ( getAnalytics
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.State (gets, liftIO)
import qualified Database.PostgreSQL.Simple as P

import USh.Utils

-- | gets the analytics for a given short URL
getAnalytics :: String          -- ^ short URL
             -> MainError Int   -- ^ amount of clicks
getAnalytics sURL = do
  pc <- gets asPostgresConnect
  lnk <- liftIO $ P.query pc "SELECT * FROM urls WHERE short_url = ?" (P.Only sURL)
  case lnk of
    [r] -> return $ sClickCount r
    [] -> throwError $ UShError USh404 "getAnalytics: the short URL is not in the database"
    _  -> throwError $ UShError UShUnreachable "getAnalytics: the short URL is in the database more than once"

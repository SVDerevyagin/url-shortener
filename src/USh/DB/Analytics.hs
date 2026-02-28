{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Encapsulates getting the usage analytics

module USh.DB.Analytics
  ( getAnalytics
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.State (gets, liftIO)
import qualified Data.Text as T

import qualified Hasql.Session as HS
import qualified Hasql.TH      as TH


import USh.Utils

-- | gets the analytics for a given short URL
getAnalytics :: String          -- ^ short URL
             -> MainError Int   -- ^ amount of clicks
getAnalytics sURL = do
  pc <- gets asPostgresConnect
  let selectUrl = HS.statement (T.pack sURL) [TH.maybeStatement|SELECT click_count::int4 FROM urls WHERE short_url = $1::text|]
  lnk <- liftIO $ HS.run selectUrl pc
  case lnk of
    Right (Just r)  -> return $ fromIntegral r
    Right (Nothing) -> throwError $ UShError USh404 "getAnalytics: the short URL is not in the database"
    Left err        -> throwError $ UShError UShUnreachable $ "getAnalytics: " ++ show err

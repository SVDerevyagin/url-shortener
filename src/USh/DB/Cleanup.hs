{-# LANGUAGE OverloadedStrings #-}

-- | Encapsulates deleting data from the databases

module USh.DB.Cleanup
  ( removeShortURL
  , cleanup
  ) where

import Control.Monad (void)
import Control.Monad.State (gets, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Time
import qualified Database.PostgreSQL.Simple as P
import qualified Database.Redis as R

import USh.Utils

-- | removes a short URL from Redis cache and the =urls= table
-- also marks the key as unused in the =keys= table
removeShortURL :: String        -- ^ short URL
               -> MainError ()
removeShortURL sURL = do
  rc <- gets asRedisConnect
  let rkey = BS.pack $ "short:" ++ sURL
  void $ liftIO $ R.runRedis rc $ R.del [rkey]

  pc <- gets asPostgresConnect
  void $ liftIO $ P.execute pc "DELETE FROM urls WHERE short_url = ?" (P.Only sURL)
  void $ liftIO $ P.execute pc "UPDATE keys SET used = False WHERE key = ?" (P.Only sURL)

-- | deletes all expired urls
cleanup :: MainError ()
cleanup = do
  now <- liftIO getCurrentTime
  pc <- gets asPostgresConnect
  expLinks <- liftIO $ P.query pc "SELECT * FROM urls WHERE expiration_date < ?" (P.Only now)

  rc <- gets asRedisConnect
  let rkeys = map (BS.pack . ("short:" ++) . sShortURL) expLinks
  void $ liftIO $ R.runRedis rc $ R.del rkeys

  void $ liftIO $ P.execute pc "DELETE FROM urls WHERE short_url in ?" (P.Only $ P.In $ map sShortURL expLinks)
  void $ liftIO $ P.execute pc "UPDATE keys SET used = False WHERE key in ?" (P.Only $ P.In $ map sShortURL expLinks)

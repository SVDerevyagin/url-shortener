{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Encapsulates deleting data from the databases

module USh.DB.Cleanup
  ( removeShortURL
  , cleanup
  ) where

import Control.Monad (void)
import Control.Monad.Except (throwError)
import Control.Monad.State (gets, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Time
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Vector as V

import qualified Hasql.Session  as HS
import qualified Hasql.TH       as TH
import qualified Database.Redis as R

import USh.Utils

-- | removes a short URL from Redis cache and the =urls= table
-- also marks the key as unused in the =keys= table
removeShortURL :: Text          -- ^ short URL
               -> MainError ()
removeShortURL sURL = do
  rc <- gets asRedisConnect
  let rkey = BS.pack $ T.unpack $ "short:" <> sURL
  void $ liftIO $ R.runRedis rc $ R.del [rkey]

  pc <- gets asPostgresConnect
  let deleteUrls = HS.statement sURL [TH.resultlessStatement| DELETE FROM urls                  WHERE short_url = $1::text |]
      updateKeys = HS.statement sURL [TH.resultlessStatement| UPDATE      keys SET used = False WHERE       key = $1::text |]
  void $ liftIO $ HS.run deleteUrls pc
  void $ liftIO $ HS.run updateKeys pc

-- | deletes all expired urls
cleanup :: MainError ()
cleanup = do
  now <- liftIO getCurrentTime
  pc <- gets asPostgresConnect
  let selectOld = HS.statement now [TH.vectorStatement| SELECT short_url::text FROM urls WHERE expiration_date < $1::timestamptz |]
  expLinks' <- liftIO $ HS.run selectOld pc

  rc <- gets asRedisConnect
  case expLinks' of
    Left err       -> throwError $ UShError UShPostgresError $ "cleanup: " ++ show err
    Right els -> do
      let expLinks = V.toList els
          rkeys = map (BS.pack . ("short:" ++) . T.unpack) expLinks
      void $ liftIO $ R.runRedis rc $ R.del rkeys

      let deleteOlds = HS.statement els [TH.resultlessStatement| DELETE FROM urls                  WHERE short_url = ANY($1::text[]) |]
          updateKeys = HS.statement els [TH.resultlessStatement| UPDATE      keys SET used = False WHERE       key = ANY($1::text[]) |]
      void $ liftIO $ HS.run deleteOlds pc
      void $ liftIO $ HS.run updateKeys pc

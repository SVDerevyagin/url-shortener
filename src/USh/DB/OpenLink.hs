{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Encapsulates resolving a short URL

module USh.DB.OpenLink
  ( openShortURL
  ) where

import Control.Monad (when, void)
import Control.Monad.Except (throwError)
import Control.Monad.State (gets, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Time
import qualified Data.Text as T

import qualified Hasql.Session  as HS
import qualified Hasql.TH       as TH
import qualified Database.Redis as R

import USh.Utils
import USh.DB.Cleanup

-- | checks if the short URL is in Redis
--
-- if it is, checks the expiration date (and removes the URL if it’s expired)
--           updates analytics
--           returns the found original link
--
-- otherwise, checks Postgres
openShortURL :: String            -- ^ short URL
             -> MainError String  -- ^ original URL
openShortURL sURL = do
  let rkey = BS.pack $ "short:" ++ sURL
  rc <- gets asRedisConnect
  eDate <- liftIO $ R.runRedis rc $ R.hget rkey "expiration_date"
  now <- liftIO getCurrentTime
  case eDate of
    Left err -> throwError $ UShError UShRedisError $ show err
    Right dt -> case dt of
      Nothing -> return ()
      Just d  -> when (utctDay(read $ BS.unpack d) < utctDay now) $ removeShortURL sURL
  oURL <- liftIO $ R.runRedis rc $ R.hget rkey "original_url"
  case oURL of
    Left err -> throwError $ UShError UShRedisError $ show err
    Right result -> case result of
      Nothing   -> openShortURL' sURL
      Just link -> do
        pc <- gets asPostgresConnect
        let updateUrls = HS.statement (T.pack sURL) [TH.resultlessStatement| UPDATE urls SET click_count = click_count + 1 WHERE short_url = $1::text |]
        void $ liftIO $ HS.run updateUrls pc
        return $ BS.unpack link

-- | checks if the short URL is in Postgres
--
-- if it is, checks the expiration date (and removes the URL if it’s expired)
--           updates analytics
--           returns the found original link
--
-- otherwise return USh404 error
openShortURL' :: String            -- ^ short URL
              -> MainError String  -- ^ original URL
openShortURL' sURL = do
  pc <- gets asPostgresConnect
  let selectUrl = HS.statement (T.pack sURL) [TH.maybeStatement|SELECT expiration_date::timestamptz, original_url::text FROM urls WHERE short_url = $1::text|]
  result <- liftIO $ HS.run selectUrl pc
  case result of
    Left err      -> throwError $ UShError UShUnreachable $ "openShortURL: " ++ show err
    Right Nothing -> throwError $ UShError USh404 "openShortURL: the short URL is not in the database"
    Right (Just (expDate, oURL)) -> do
      now <- liftIO getCurrentTime
      let eDay = utctDay expDate
      if eDay < utctDay now then do
        removeShortURL sURL
        throwError $ UShError USh404 "openShortURL: the short URL is not in the database"
      else do
        let updateUrl = HS.statement (T.pack sURL) [TH.resultlessStatement|UPDATE urls SET click_count = click_count + 1 WHERE short_url = $1::text|]
        void $ liftIO $ HS.run updateUrl pc
        return $ T.unpack oURL

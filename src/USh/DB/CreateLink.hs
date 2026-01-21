{-# LANGUAGE OverloadedStrings #-}

-- | Encapsulates creating a short URL
module USh.DB.CreateLink
  ( createShortURL
  ) where

import Control.Monad (void)
import Control.Monad.Except (throwError)
import Control.Monad.State (gets, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.List (isPrefixOf)
import Data.Time
import qualified Database.PostgreSQL.Simple as P
import qualified Database.Redis as R

import USh.Utils

-- | creates a short URL
--
-- 1. if custom short url is given, checks if it is in use;
-- otherwise gets a short url from Redis
-- 2. sets expiration date
-- 3. makes request to the database
createShortURL :: String              -- ^ original url
               -> Maybe String        -- ^ custom short url
               -> Maybe UTCTime       -- ^ custom expiration date
               -> MainError URL
createShortURL oURL sURL eDate = do
  used <- case sURL of
    Nothing -> return False
    Just l  -> checkURL l
  if used then throwError $ UShError UShShortLinkExists "provided custom short url is already in use"
    else do
      shortLink      <- maybe getShortLink return sURL
      expirationDate <- maybe getExpDate   return eDate
      let originalLink = (if "http" `isPrefixOf` oURL then "" else "https://") ++ oURL
      createShortURL' originalLink shortLink expirationDate

-- | makes request to the database
--
-- 1. inserts into Postgres a new link
-- 2. inserts into Redis cached link
createShortURL' :: String         -- ^ original url
                -> String         -- ^ custom short url
                -> UTCTime        -- ^ custom expiration date
                -> MainError URL
createShortURL' oURL sURL eDate = do
  pc <- gets asPostgresConnect
  shL' <- liftIO $ P.query pc "INSERT INTO urls(short_url, original_url, expiration_date) VALUES (?, ?, ?) RETURNING *"
                        (sURL, oURL, eDate)
  shL@(URL rkey' _ _ expDate _) <- case shL' :: [URL] of
    [sl] -> return sl
    _    -> throwError $ UShError UShPostgresError "createShortURL: INSERT INTO returned something wrong"
  let rkey = BS.pack $ "short:" ++ rkey'

  rc <- gets asRedisConnect
  status <- liftIO $ R.runRedis rc $ R.hmset rkey [ ("original_url",    BS.pack oURL)
                                                  , ("expiration_date", BS.pack $ show expDate)
                                                  ]
  case status of
    Left err     -> throwError $ UShError UShRedisError $ show err
    Right result -> case result of
      R.Ok         -> setExpTime shL
      R.Pong       -> throwError $ UShError UShRedisError "createShortURL: hmset returned Pong"
      R.Status err -> throwError $ UShError UShRedisError $ BS.unpack err

-- | sets expiration time for the link in Redis
setExpTime :: URL -> MainError URL
setExpTime res@(URL l _ _ _ _) = do
  rc <- gets asRedisConnect
  void $ liftIO $ R.runRedis rc $ R.expire (BS.pack l) $ 7*24*60*60
  return res

-- | gets an unused short link
--
-- 1. repopulates the Redis cache of short URLs if needed
-- 2. takes a short URL from the Redis cache
getShortLink :: MainError String
getShortLink = do
  rc <- gets asRedisConnect
  redisLoadKeys
  key <- liftIO $ R.runRedis rc $ R.spop cashedKeys
  case key of
    Left err     -> throwError $ UShError UShRedisError $ show err
    Right result -> case result of
      Nothing -> throwError $ UShError UShRedisError "getShortLink: spop cashedKeys returned Nothing"
      Just k  -> return $ BS.unpack k

-- | default expiration date is one year from now
getExpDate :: MainError UTCTime
getExpDate = liftIO $ addYears 1 <$> getCurrentTime

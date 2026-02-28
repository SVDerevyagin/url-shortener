{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Some utilities

module USh.Utils
  ( AppState(..)
  , UShErrorType(..), UShError(..)
  , MainError, runMain, connect, closeApp
  , URL(..)
  , redisLoadKeys, cashedKeys
  , checkURL
  , addYears, dayToUtc
  ) where

import qualified Hasql.Connection                    as H
import qualified Hasql.Connection.Setting            as H
import qualified Hasql.Connection.Setting.Connection as HSt
import qualified Hasql.Session   as HS
import qualified Hasql.TH        as TH
import qualified Database.Redis as R

import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Vector as V
import Control.Monad.State (StateT, evalStateT, gets)
import Control.Monad.Except (ExceptT, runExceptT, MonadError (throwError))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Maybe (isNothing, isJust)
import Data.Time (UTCTime(..), Day, addGregorianYearsRollOver)
import System.Environment (getEnv)
import Control.Concurrent (forkIO)
import Control.Monad (when, void)
import Control.Exception (bracket, throwIO, Exception)
import GHC.Generics (Generic)

-- | Application state keeps connections to the Postgres and Redis databases
data AppState = AppState
  { asPostgresConnect :: !H.Connection
  , asRedisConnect :: !R.Connection
  }

-- | Types of error
data UShErrorType = UShRedisError        -- ^ An error on Redis side
                  | UShPostgresError     -- ^ An error on Postgres side
                  | UShShortLinkExists   -- ^ Creating an existing link
                  | USh404               -- ^ The short link is not in the database
                  | UShUnreachable       -- ^ An error that cannot happen but =case of= needs something to be complete
                  deriving (Show, Eq)

-- | Error datatype
data UShError = UShError
  { ueType :: !UShErrorType   -- ^ type of error
  , ueMsg  :: !String         -- ^ message
  } deriving (Show, Eq, Exception)

--instance MonadError UShError IO where
--instance MonadError UShError MainError where

-- | throw 'UShPostgresError'
thrPE :: (Show a, MonadError UShError m) => a -> m b
thrPE = throwError . UShError UShPostgresError . show

-- | throwIO 'UShPostgresError'
thrPEio :: (Show a) => a -> IO b
thrPEio = throwIO . UShError UShPostgresError . show


-- | Main state monad of the application: connections to the databases
type MainState = StateT AppState IO
-- | Error monad
type MainError = ExceptT UShError MainState

-- | runs the application monad
runMain :: Bool            -- ^ True: main mode, False: test mode
        -> MainError a
        -> IO (Either UShError a)
runMain isMain me = do
  bracket
    (connect isMain)
    closeApp
    (evalStateT (runExceptT me))

-- | A record from =urls= table
data URL = URL
  { sShortURL       :: !String
  , sOriginalURL    :: !String
  , sCreationDate   :: !UTCTime
  , sExpirationDate :: !UTCTime
  , sClickCount     :: !Int
  } deriving (Show, Eq, Generic)

-- | Connects to the databases
connect :: Bool          -- ^ True: main mode, False: test mode
        -> IO AppState
connect isMain = do
  -- setup variables
  let (pUserString, rDb) = if isMain then ("DB_MAIN_USER", 0) else ("DB_TEST_USER", 1)
  pUser <- getEnv pUserString
  rp    <- getEnv "R_PORT"

  -- connection to Postgres
  let pcString = "host=localhost port=5432 user=" <> T.pack pUser <> " dbname=" <> T.pack pUser
      pci = [H.connection $ HSt.string pcString]

  ep <- H.acquire pci
  p <- case ep of
    Left err -> thrPEio err
    Right c  -> return c
  -- populates =keys= table
  populateKeys p isMain

  -- connection to Redis
  let rci = R.defaultConnectInfo { R.connectPort = R.PortNumber $ read rp
                                 , R.connectDatabase = rDb }
  r <- R.checkedConnect rci

  let as = AppState p r
  -- load keys to Redis if necessary
  void $ evalStateT (runExceptT redisLoadKeys) as
  return as

-- | closes connections to the databases
closeApp :: AppState -> IO ()
closeApp (AppState p r) = do
  H.release p
  R.disconnect r

-- | all possible 3-character alphanumerical strings
allKeys :: [Text]
allKeys = [ T.pack [a,b,c] | a<-letters, b<-letters, c<-letters ]
  where
    letters = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

-- | populates =keys= table with possible short URLs
populateKeys :: H.Connection -> Bool -> IO ()
populateKeys conn isMain = if isMain then populateMainKeys conn else populateTestKeys conn allKeys

-- | all keys in the main database should be
-- `mainKeys = concatMap (\s -> map (++s) allKeys) allKeys`
-- but this list is too big
-- it forks the main loop of inserting the keys into the =keys= table
populateMainKeys :: H.Connection -> IO ()
populateMainKeys conn = do
  void $ HS.run session conn
  void $ forkIO $ mapM_ (populatePartialMainKeys conn) $ zip allKeys $ map (\s -> map (<>s) allKeys) allKeys
  where
    session = HS.sql [TH.uncheckedSqlFile|data/migration.sql|]

-- | inserts some of the keys into the =keys= table and records their suffix
populatePartialMainKeys :: H.Connection
                        -> (Text, [Text])  -- ^ (a suffix, list of keys with the suffix)
                        -> IO ()
populatePartialMainKeys conn (key,keys) = do
  res <- HS.run checkTheKey conn
  case res of
    Left err -> thrPEio err
    Right r  -> when (isNothing r) $ void $ HS.run insertKeys conn
  where
    checkTheKey = HS.statement key [TH.maybeStatement| SELECT key :: text? FROM created_keys WHERE key = ($1::text) |]
    insertKeys = do
      HS.sql [TH.uncheckedSql| BEGIN |]
      HS.statement key [TH.resultlessStatement| INSERT INTO created_keys (key) VALUES ($1::text) |]
      liftIO $ populateSomeKeys conn keys
      HS.sql [TH.uncheckedSql| COMMIT |]

-- | checks if the =keys= table is empty,
--   inserts the possible short URL into the empty =keys= table
populateTestKeys :: H.Connection -> [Text] -> IO ()
populateTestKeys conn keys = do
  ks <- HS.run checkTheKeys conn
  case ks of
    Left err -> thrPEio err
    Right k  -> when (V.null k) $ populateSomeKeys conn keys
  where
    checkTheKeys = HS.statement () [TH.vectorStatement| SELECT key :: text FROM keys |]

-- | inserts the possible short URL into the =keys= table
populateSomeKeys :: H.Connection -> [Text] -> IO ()
populateSomeKeys conn keys = do
  void $ HS.run insertKeys conn
  --return ()
  where
    insertKeys = HS.statement (V.fromList keys) [TH.resultlessStatement| INSERT INTO keys (key) SELECT * FROM unnest ($1::text[]) |]

-- | key for using in the Redis database
cashedKeys :: ByteString
cashedKeys = "cashed_keys"

-- | if there are no cashed keys in Redis
--   takes unused keys from the Postgres database and caches them in the Redis database
redisLoadKeys :: MainError ()
redisLoadKeys = do
  pc <- gets asPostgresConnect
  rc <- gets asRedisConnect
  liftIO $ R.runRedis rc $ do
    n <- R.scard cashedKeys
    when (n == Right 0) $ do
      let getKeys = HS.statement () [TH.singletonStatement|
                        WITH selected_keys AS (SELECT key FROM keys WHERE NOT used ORDER BY random_val LIMIT 1000)
                        UPDATE keys SET used = true, random_val = random() FROM selected_keys WHERE keys.key = selected_keys.key
                        RETURNING keys.key :: bytea[]
                      |]
      keys <- liftIO $ HS.run getKeys pc
      void $ case keys of
        Left err -> liftIO $ throwIO $ UShError UShPostgresError $ show err -- thrPE err
        Right ks -> R.sadd cashedKeys $ V.toList ks
      --void $ R.sadd cashedKeys keys
  return ()


-- | checks if a given short link is in the =urls= table
--
--   True  = the link is present,
--   False = the link is absent
checkURL :: Text
         -> MainError Bool
checkURL link = do
  conn <- gets asPostgresConnect
  let getShortUrl = HS.statement link [TH.maybeStatement| SELECT short_url::text FROM urls WHERE short_url = $1::text|]
  used <- liftIO $ HS.run getShortUrl conn
  case used of
    Left err -> thrPE err
    Right u  -> return $ isJust u
  --return $ isJust used


-- | adds integer number of years to a given date-time
addYears :: Integer -> UTCTime -> UTCTime
addYears n (UTCTime d _) = dayToUtc $ addGregorianYearsRollOver n d

-- | converts 'Day' to 'UTCTime' with 0 as time part
dayToUtc :: Day -> UTCTime
dayToUtc day = UTCTime day 0

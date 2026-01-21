{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

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

import qualified Database.PostgreSQL.Simple as P
import qualified Database.Redis as R
import Control.Monad.State (StateT, evalStateT, gets)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.String (fromString)
import Data.Time (UTCTime(..), Day, addGregorianYearsRollOver)
import System.Environment (getEnv)
import Control.Concurrent (forkIO)
import Control.Monad (when, void)
import Control.Exception (bracket)
import GHC.Generics (Generic)

-- | Application state keeps connections to the Postgres and Redis databases
data AppState = AppState
  { asPostgresConnect :: !P.Connection
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
  } deriving (Show, Eq)

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
  } deriving (Show, Generic, P.FromRow)

-- | Connects to the databases
connect :: Bool          -- ^ True: main mode, False: test mode
        -> IO AppState
connect isMain = do
  -- setup variables
  let (pUserString, rDb) = if isMain then ("DB_MAIN_USER", 0) else ("DB_TEST_USER", 1)
  pUser <- getEnv pUserString
  rp    <- getEnv "R_PORT"

  -- connection to Postgres
  let pci = P.defaultConnectInfo { P.connectUser = pUser
                                 , P.connectDatabase = pUser
                                 }
  p <- P.connect pci
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
  P.close p
  R.disconnect r

-- | all possible 3-character alphanumerical strings
allKeys :: [String]
allKeys = [ [a,b,c] | a<-letters, b<-letters, c<-letters ]
  where
    letters = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

-- | populates =keys= table with possible short URLs
populateKeys :: P.Connection -> Bool -> IO ()
populateKeys conn isMain = if isMain then populateMainKeys conn else populateTestKeys conn allKeys

-- | all keys in the main database should be
-- `mainKeys = concatMap (\s -> map (++s) allKeys) allKeys`
-- but this list is too big
-- it forks the main loop of inserting the keys into the =keys= table
populateMainKeys :: P.Connection -> IO ()
populateMainKeys conn = do
  void $ P.execute_ conn "CREATE TABLE IF NOT EXISTS created_keys(key TEXT PRIMARY KEY)"
  void $ forkIO $ mapM_ (populatePartialMainKeys conn) $ zip allKeys $ map (\s -> map (++s) allKeys) allKeys

-- | inserts some of the keys into the =keys= table and records their suffix
populatePartialMainKeys :: P.Connection
                        -> (String, [String])  -- ^ (a suffix, list of keys with the suffix)
                        -> IO ()
populatePartialMainKeys conn (key,keys) = do
  res <- P.query conn "SELECT key FROM created_keys WHERE key = ?" (P.Only key)
  when (null (res :: [P.Only String])) $ do
    P.begin conn
    void $ P.execute conn "INSERT INTO created_keys(key) VALUES (?)" (P.Only key)
    populateSomeKeys conn keys
    P.commit conn
    return ()

-- | checks if the =keys= table is empty,
--   inserts the possible short URL into the empty =keys= table
populateTestKeys :: P.Connection -> [String] -> IO ()
populateTestKeys conn keys = do
  ks <- P.query_ conn "SELECT key FROM keys"
  when (null (ks :: [P.Only String])) $ populateSomeKeys conn keys

-- | inserts the possible short URL into the =keys= table
populateSomeKeys :: P.Connection -> [String] -> IO ()
populateSomeKeys conn keys = do
  _ <- P.executeMany conn "INSERT INTO keys(key) VALUES (?)" $ map P.Only keys
  return ()

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
      keys' <- liftIO $ P.query_ pc $ fromString
          ( "WITH selected_keys AS (SELECT key FROM keys WHERE NOT used ORDER BY random_val LIMIT 1000) "
         ++ "UPDATE keys SET used = true, random_val = random() FROM selected_keys WHERE keys.key = selected_keys.key RETURNING keys.key"
          )
      let keys = map P.fromOnly keys'
      void $ R.sadd cashedKeys keys
  return ()


-- | checks if a given short link is in the =urls= table
--
--   True  = the link is present,
--   False = the link is absent
checkURL :: String
         -> MainError Bool
checkURL link = do
  conn <- gets asPostgresConnect
  used <- liftIO $ P.query conn "SELECT short_url FROM urls WHERE short_url = ?" (P.Only link)
  return $ not $ null (used :: [P.Only String])


-- | adds integer number of years to a given date-time
addYears :: Integer -> UTCTime -> UTCTime
addYears n (UTCTime d _) = dayToUtc $ addGregorianYearsRollOver n d

-- | converts 'Day' to 'UTCTime' with 0 as time part
dayToUtc :: Day -> UTCTime
dayToUtc day = UTCTime day 0

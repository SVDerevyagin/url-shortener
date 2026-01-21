{-# LANGUAGE OverloadedStrings #-}

-- | Testing 'USh.DB' module
module Tests.DB
  ( testCreateShortURL
  , testOpenShortURL
  , testGetAnalytics
  ) where

import Control.Monad (replicateM_, void)
import Control.Monad.State (evalStateT, gets, liftIO)
import Control.Monad.Except (runExceptT)

import Data.Time
import qualified Database.PostgreSQL.Simple as P
import Test.Hspec
import Test.QuickCheck

import USh.DB
import USh.Utils

-- | generates a random date in 2026
genDate :: Gen UTCTime
genDate = do
  m <- arbitrary `suchThat` (\x -> x > 0 && x <= 12)
  d <- arbitrary `suchThat` (\x -> x > 0 && x <= 28)
  let day = fromGregorian 2026 m d
  return $ dayToUtc day

-- | generates a random short URL length 10 to 15
genLink :: Gen String
genLink = do
  n <- arbitrary `suchThat` (\x -> x > 10 && x <= 15)
  vectorOf n genChar

-- | generates a random alphanumeric character
genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- | unwraps the main monad
runMain' :: AppState -> MainError a -> IO (Either UShError a)
runMain' as me = evalStateT (runExceptT me) as

-- | tests 'createShortURL'
testCreateShortURL :: SpecWith AppState
testCreateShortURL = do
  describe "createShortURL" $ do
    it "default use" $ \as -> do
      result <- runMain' as $ createShortURL "example.com" Nothing Nothing
      case result of
        Left err -> expectationFailure $ "unexpected error: " ++ show err
        Right sl -> do
          void $ runMain' as $ removeShortURL (sShortURL sl)
          sOriginalURL sl `shouldBe` "https://example.com"

    it "given expiration date" $ \as -> do
      forAll genDate $ \eDate -> do
        result <- runMain' as $ createShortURL "example.com" Nothing (Just eDate)
        case result of
          Left err -> expectationFailure $ "unexpected error: " ++ show err
          Right sl -> do
            void $ runMain' as $ removeShortURL (sShortURL sl)
            sOriginalURL sl `shouldBe` "https://example.com"
            sExpirationDate sl `shouldBe` eDate

    it "given link name (not used)" $ \as -> do
      forAll genLink $ \link -> do
        result <- runMain' as $ createShortURL "example.com" (Just link) Nothing
        case result of
          Left err -> expectationFailure $ "unexpected error: " ++ show err
          Right sl -> do
            void $ runMain' as $ removeShortURL link
            sOriginalURL sl `shouldBe` "https://example.com"
            sShortURL sl `shouldBe` link

    it "given link name (used)" $ \as -> do
      forAll genLink $ \link -> do
        now <- getCurrentTime
        result <- runMain' as $ do
          pc <- gets asPostgresConnect
          void $ liftIO $ P.execute pc "INSERT INTO urls(short_url, original_url, expiration_date) VALUES (?, ?, ?)" (link, "example.com"::String, now)
          createShortURL "example.com" (Just link) Nothing
        void $ runMain' as $ removeShortURL link
        case result of
          Right _  -> expectationFailure "unexpected success: "
          Left (UShError UShShortLinkExists _) -> return ()
          Left (UShError _ err) -> expectationFailure $ "unexpected error: " ++ show err


-- | tests 'openShortURL'
testOpenShortURL :: SpecWith AppState
testOpenShortURL = do
  describe "openShortURL" $ do
    it "link exists in the db" $ \as -> do
      url <- runMain' as $ do
        sl <- createShortURL oURL Nothing Nothing
        url <- openShortURL (sShortURL sl)
        removeShortURL (sShortURL sl)
        return url
      case url of
        Left err -> expectationFailure $ "unexpected error: " ++ show err
        Right l  -> l `shouldBe` oLnk

    it "link doesn’t exist in the db" $ \as -> do
      forAll genLink $ \link -> do
        ourl <- runMain' as $ openShortURL link
        case ourl of
          Right l  -> expectationFailure $ "unexpected success: " ++ l
          Left (UShError USh404 _) -> return ()
          Left err -> expectationFailure $ "unexpected error: " ++ show err

    it "link expired" $ \as -> do
      now <- getCurrentTime
      Right sl <- runMain' as $ createShortURL oURL Nothing (Just $ addYears (-1) now)
      l <- runMain' as $ openShortURL (sShortURL sl)
      Right pc <- runMain' as $ gets asPostgresConnect
      u  <- P.query pc "SELECT * FROM urls WHERE short_url = ?" (P.Only $ sShortURL sl)
      k  <- P.query pc "SELECT used FROM keys WHERE key = ?" (P.Only $ sShortURL sl)
      void $ runMain' as $ removeShortURL (sShortURL sl)
      case l of
        Right ou -> expectationFailure $ "unexpected success: " ++ show ou
        Left (UShError USh404 _) -> do
          length (u::[URL]) `shouldBe` 0
          k `shouldBe` [P.Only False]
        Left err -> expectationFailure $ "unexpected error: " ++ show err
  where
    oURL = "example.com"
    oLnk = "https://example.com"


-- | tests 'getAnalytics'
testGetAnalytics :: SpecWith AppState
testGetAnalytics = do
  describe "getAnalytics" $ do
    it "check" $ \as -> do
      forAll (arbitrary `suchThat` (>0)) $ \n -> do
        forAll genLink $ \link -> do
          m <- runMain' as $ do
            surl <- createShortURL link Nothing Nothing
            let sURL = sShortURL surl
            replicateM_ n $ openShortURL sURL
            m <- getAnalytics sURL
            removeShortURL sURL
            return m
          case m of
            Left err -> expectationFailure $ "unexpected error: " ++ show err
            Right i  -> i `shouldBe` n

{-# LANGUAGE OverloadedStrings #-}

-- | Testing 'USh.Utils' module
module Tests.Utils
  ( testCheckURL
  ) where

import Control.Monad (void)
import Control.Monad.State (evalStateT, gets, liftIO)
import Control.Monad.Except (runExceptT)

import Data.Time
import qualified Database.PostgreSQL.Simple as P
import Test.Hspec
import Test.QuickCheck

import USh.Utils


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

-- | tests 'checkURL'
testCheckURL :: SpecWith AppState
testCheckURL = do
  describe "checkURL" $ do
    it "link is in the database" $ \as -> do
      forAll genLink $ \link -> do
        now <- getCurrentTime
        r <- runMain' as $ do
          pc <- gets asPostgresConnect
          void $ liftIO
            $ P.execute pc "INSERT INTO urls(short_url, original_url, expiration_date) VALUES (?, ?, ?)"
                            (link, "https://example.com"::String, now)
          res <- checkURL link
          void $ liftIO $ P.execute pc "DELETE FROM urls WHERE short_url = ?" (P.Only link)
          return res
        r `shouldBe` Right True

    it "link is not in the database" $ \as -> do
      forAll genLink $ \link -> do
        r <- runMain' as $ checkURL link
        r `shouldBe` Right False

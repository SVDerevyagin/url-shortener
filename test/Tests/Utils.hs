{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Testing 'USh.Utils' module
module Tests.Utils
  ( testCheckURL
  ) where

import Control.Monad (void)
import Control.Monad.State (evalStateT, gets, liftIO)
import Control.Monad.Except (runExceptT)
import Data.Time
import qualified Data.Text as T
import           Data.Text (Text)

import qualified Hasql.Session  as HS
import qualified Hasql.TH       as TH

import Test.Hspec
import Test.QuickCheck

import USh.Utils


-- | generates a random short URL length 10 to 15
genLink :: Gen Text
genLink = do
  n <- arbitrary `suchThat` (\x -> x > 10 && x <= 15)
  vectorOf n genChar >>= return . T.pack

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
          let insertUrl = HS.statement (link, "https://example.com", now)
                            [TH.resultlessStatement|INSERT INTO urls (short_url, original_url, expiration_date)
                                                   VALUES ($1::text, $2::text, $3::timestamptz)|]
              deleteUrl = HS.statement link
                            [TH.resultlessStatement|DELETE FROM urls WHERE short_url = $1::text|]
          void $ liftIO $ HS.run insertUrl pc
          res <- checkURL link
          void $ liftIO $ HS.run deleteUrl pc
          return res
        r `shouldBe` Right True

    it "link is not in the database" $ \as -> do
      forAll genLink $ \link -> do
        r <- runMain' as $ checkURL link
        r `shouldBe` Right False

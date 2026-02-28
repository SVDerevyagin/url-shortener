{-# LANGUAGE OverloadedStrings #-}

-- | Scotty application

module ScottyApp
  ( runApp
  , app
  ) where

import           Data.Aeson (Value(..), object, (.=))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Time
import           Network.Wai (Application)
import qualified Web.Scotty as S

import USh.DB
import USh.Utils

-- | describes all available routes of the server
app' :: S.ScottyM ()
app' = do
  S.post "/shorten" $ do
    url   <- S.formParam      "url"
    alias <- S.formParamMaybe "alias"
    edate <- S.formParamMaybe "edate"
    eDate <- traverse (parseTimeM True defaultTimeLocale "%Y-%m-%d") edate
    shortLink <- S.liftIO $ runMain True $ createShortURL url alias (dayToUtc <$> eDate)
    case shortLink of
      Right sl -> S.json $ object [ "short_link" .= String (sShortURL sl)
                                  , "creation_date" .= String (T.pack $ showGregorian $ utctDay $ sCreationDate sl)
                                  , "expiration_date" .= String (T.pack $ showGregorian $ utctDay $ sExpirationDate sl)
                                  ]
      Left (UShError UShShortLinkExists msg) -> S.json $ object [ "error" .= String (T.pack msg) ]
      Left err -> S.json $ object [ "error" .= String (T.pack $ show err) ]
  S.get "/analytics/:url" $ do
    url <- S.captureParam "url"
    analytics <- S.liftIO $ runMain True $ getAnalytics url
    case analytics of
      Right n  -> S.json $ object [ "click_count" .= String (T.pack $ show n) ]
      Left (UShError USh404 _) -> S.redirect "/error404"
      Left err -> S.json $ object [ "error" .= String (T.pack $ show err) ]

  S.get "/error404" $ do
    S.json $ object [ "error" .= String "404 Error: this link is not in the system"
                    ]

  S.get "/:short" $ do
    sh <- S.captureParam "short"
    Right url <- S.liftIO $ runMain True $ openShortURL sh
    S.redirect (TL.pack $ T.unpack url)

-- | can be used for testing
app :: IO Application
app = S.scottyApp app'

-- | runs the scotty app
runApp :: IO ()
runApp = S.scotty 2020 app'

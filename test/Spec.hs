
import Test.Hspec

import Control.Exception (bracket)
import Tests.Utils
import Tests.DB

import USh.Utils

-- | the 'main' function
main :: IO ()
main
  = hspec
  $ around useAppState
  $ do
  testCheckURL
  testCreateShortURL
  testOpenShortURL
  testGetAnalytics

-- | wrapper around tests
-- connects to the databases, runs the test, closes the connections
useAppState :: (AppState -> IO ()) -> IO ()
useAppState action = do
  bracket
    (connect False)
    closeApp
    action

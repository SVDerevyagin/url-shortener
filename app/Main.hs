{-# LANGUAGE OverloadedStrings #-}

-- | The server app
module Main (main) where

import ScottyApp

-- | the `main` function
main :: IO ()
main = runApp

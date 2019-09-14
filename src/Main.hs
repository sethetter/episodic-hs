{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Web.Scotty
import qualified Episodic.Data as Data
import qualified Data.Text.Lazy as T
import           Control.Monad.IO.Class

main :: IO ()
main = do
  Data.loadSchema
  Data.loadSeedData
  scotty 3000 app

app :: ScottyM ()
app = do
  get "/" $ html "<h1>Sup?!</h1>"

  get "/data" $ do
    e <- liftIO Data.getEpisode
    html $ mconcat ["<h1>", T.pack $ show e, "</h1>"]

  get "/watch/refresh" $ do
    Data.refreshWatchList

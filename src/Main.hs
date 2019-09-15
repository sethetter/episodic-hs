{-# LANGUAGE DeriveGeneric,OverloadedStrings #-}
module Main where

import qualified Web.Scotty as Scotty
import qualified Episodic.Data as Data
import qualified Data.List as List
import           Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import           GHC.Generics
import           Data.Text.Lazy as T
import           Control.Monad.IO.Class

main :: IO ()
main = do
  Data.loadSchema
  Data.loadSeedData
  Scotty.scotty 3000 app

app :: Scotty.ScottyM ()
app = do
  Scotty.get "/" $ do
    watchlist <- liftIO Data.getWatchList
    let wlHTML = List.foldl (\o e -> o ++ "<li>" ++ show e ++ "</li>") "" watchlist
    Scotty.html $ mconcat ["<ul>", T.pack wlHTML, "</ul>"]

  Scotty.get "/watchlist" $ do
    watchlist <- liftIO Data.getWatchList
    Scotty.json watchlist

  Scotty.post "/watchlist/add" $ do
    body <- Scotty.body
    case (JSON.decode body) :: Maybe Data.NewWatchListItem of
      Nothing -> Scotty.json $ ErrorResponse 400 "failed to parse input"
      Just newWLI -> do
        liftIO $ Data.insertWatchListItem newWLI
        Scotty.json $ JSON.object ["message" .= ("Created!" :: Text)]

  -- get "/watch/refresh" $ do
    -- Data.refreshWatchList
    -- return data on what was added

-- TODO: json error response function?
data ErrorResponse = ErrorResponse
  { status :: Int
  , message :: Text
  } deriving (Show, Generic)

instance JSON.ToJSON ErrorResponse

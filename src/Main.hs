{-# LANGUAGE DeriveGeneric,OverloadedStrings #-}
module Main where

import qualified Web.Scotty as Scotty
import qualified Episodic.Data as Data
import qualified Data.List as List
import           Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import qualified Database.SQLite.Simple as SQLite
import           GHC.Generics
import           Data.Text.Lazy as T
import           Control.Monad.IO.Class

main :: IO ()
main = do
  db <- Data.openDB "test"
  Data.loadSchema db
  Scotty.scotty 3000 $ app db

app :: SQLite.Connection -> Scotty.ScottyM ()
app db = do
  Scotty.get "/" $ do
    watchlist <- liftIO $ Data.getWatchList db
    let wlHTML = List.foldl (\o e -> o ++ "<li>" ++ show e ++ "</li>") "" watchlist
    Scotty.html $ mconcat ["<ul>", T.pack wlHTML, "</ul>"]

  -- WatchList
  -------------------------------------

  Scotty.get "/watchlist" $ do
    watchlist <- liftIO $ Data.getWatchList db
    Scotty.json watchlist

  Scotty.post "/watchlist" $ do
    body <- Scotty.body
    case (JSON.decode body) :: Maybe Data.NewWatchListItem of
      Nothing -> Scotty.json $ ErrorResponse 400 "failed to parse input"
      Just newWLI -> do
        liftIO $ Data.insertWatchListItem db newWLI
        Scotty.json $ JSON.object ["message" .= ("Created!" :: Text)]

  -- Shows
  -------------------------------------

  Scotty.get "/shows" $ do
    shows' <- liftIO $ Data.getShows db
    Scotty.json shows'

  Scotty.post "/shows" $ do
    body <- Scotty.body
    case (JSON.decode body) :: Maybe Data.Show' of
      Nothing -> Scotty.json $ ErrorResponse 400 "failed to parse input"
      Just s -> do
        liftIO $ Data.insertShow db s
        Scotty.json $ JSON.object ["message" .= ("Created!" :: Text)]


-- Responses
--------------------------------------

data ErrorResponse = ErrorResponse
  { status :: Int
  , message :: Text
  } deriving (Show, Generic)

instance JSON.ToJSON ErrorResponse

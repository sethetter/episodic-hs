{-# LANGUAGE QuasiQuotes,OverloadedStrings #-}
module Episodic.Data where
  -- ( loadSchema
  -- , loadSeedData
  -- , refreshWatchList
  -- )

import           Data.Text
import           Text.RawString.QQ
import qualified Database.SQLite.Simple as SQLite

schema :: SQLite.Query
schema = [r|
  CREATE TABLE IF NOT EXISTS watch_list
    ( id       INTEGER PRIMARY KEY
    , type     TEXT NOT NULL
    , name     TEXT NOT NULL
    , season   INTEGER NOT NULL
    , episode  INTEGER NOT NULL
    , air_date INTEGER NOT NULL
    , show_id  INTEGER NOT NULL
    , FOREIGN KEY (show_id) REFERENCES shows(id)
    );

  CREATE TABLE IF NOT EXISTS shows
    ( id    INTEGER PRIMARY KEY
    , name  TEXT
    );
|]

loadSchema :: IO ()
loadSchema = do
  conn <- SQLite.open "test.db"
  SQLite.execute_ conn schema

loadSeedData :: IO ()
loadSeedData = do
  conn <- SQLite.open "test.db"
  SQLite.execute_ conn "INSERT INTO watch_list (id, episode) VALUES (1, 'Rick and Morty S01E01')"

-- Episode
-------------------------------------------------

data WatchListItem = WatchListItem
  { epID :: Int
  , epName :: Text
  }
  deriving (Show)

instance SQLite.FromRow WatchListItem where
  fromRow = WatchListItem <$> SQLite.field <*> SQLite.field

instance SQLite.ToRow WatchListItem where
  toRow (WatchListItem id' name) = SQLite.toRow (id', name)

getWatchList :: IO [WatchListItem]
getWatchList = do
  conn <- SQLite.open "test.db"
  watchList <- SQLite.query_ conn "SELECT * FROM watch_list"
  return watchList

-- refreshWatchList :: IO ()
-- refreshWatchList = do

  -- get all shows from the DB

{-# LANGUAGE QuasiQuotes,OverloadedStrings #-}
module Episodic.Data where ( loadSchema
                           , loadSeedData
                           , refreshWatchList
                           )

import Data.Text as T
import Database.SQLite.Simple
-- import Database.SQLite.Simple.FromRow
import Text.RawString.QQ

schema :: Query
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
  conn <- open "test.db"
  execute_ conn schema

loadSeedData :: IO ()
loadSeedData = do
  conn <- open "test.db"
  execute_ conn "INSERT INTO watch_list (id, episode) VALUES (1, 'Rick and Morty S01E01')"

-- Episode
-------------------------------------------------

data Episode = Episode
  { epID :: Int
  , epName :: Text
  } deriving (Show)

instance FromRow Episode where
  fromRow = Episode <$> field <*> field

instance ToRow Episode where
  toRow (Episode id' name) = toRow (id', name)

getEpisode :: IO Episode
getEpisode = do
  conn <- open "test.db"
  [ep] <- query_ conn "SELECT * FROM watch_list"
  return ep

refreshWatchList :: IO ()
refreshWatchList = do

  -- get all shows from the DB

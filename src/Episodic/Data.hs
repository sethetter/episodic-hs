{-# LANGUAGE QuasiQuotes,OverloadedStrings #-}
module Episodic.Data where
  -- ( loadSchema
  -- , loadSeedData
  -- , refreshWatchList
  -- )

import           Data.Time
import           Data.Text
import           Text.RawString.QQ
import           Database.SQLite.Simple (NamedParam((:=)))
import qualified Database.SQLite.Simple as SQLite
import           Data.Aeson ((.:), (.=))
import qualified Data.Aeson as JSON

schema :: SQLite.Query
schema = [r|
  CREATE TABLE IF NOT EXISTS watch_list
    ( id       INTEGER PRIMARY KEY
    , name     TEXT NOT NULL
    , air_date TEXT
    , show_id  INTEGER
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
  SQLite.execute_ conn [r|
    DELETE FROM shows;
    DELETE FROM watch_list;

    INSERT INTO shows (id, name) VALUES (66573, 'The Good Place');

    INSERT INTO watch_list (name, air_date, show_id)
      VALUES ('The Good Place (S04E08)' , '2019-09-26' , 66573);
    INSERT INTO watch_list (name)
      VALUES ('Free Solo');
  |]

-- WatchListItem
-------------------------------------------------

data WatchListItem = WatchListItem
  { wliID      :: Int
  , wliName    :: Text
  , wliAirDate :: Maybe Day
  , wliShowID  :: Maybe Int
  }
  deriving (Show)

instance SQLite.FromRow WatchListItem where
  fromRow = WatchListItem <$> SQLite.field
                          <*> SQLite.field
                          <*> SQLite.field
                          <*> SQLite.field

instance SQLite.ToRow WatchListItem where
  toRow wli = SQLite.toRow ( wliID wli
                           , wliName wli
                           , wliAirDate wli
                           , wliShowID wli
                           )

instance JSON.ToJSON WatchListItem where
  toJSON wli = JSON.object [ "id"       .= wliID wli
                           , "name"     .= wliName wli
                           , "air_date" .= wliAirDate wli
                           , "show_id"  .= wliShowID wli
                           ]

getWatchList :: IO [WatchListItem]
getWatchList = do
  conn <- SQLite.open "test.db"
  watchList <- SQLite.query_ conn "SELECT * FROM watch_list"
  return watchList

-- NewWatchListItem
-------------------------------------------------

data NewWatchListItem = NewWatchListItem
  { nwliName :: Text
  , nwliAirDate :: Maybe Text
  , nwliShowID :: Maybe Int
  }

instance JSON.FromJSON NewWatchListItem where
  parseJSON = JSON.withObject "NewWatchListItem" $ \v ->
    NewWatchListItem <$> v .: "name"
                     <*> v .: "air_date"
                     <*> v .: "show_id"

insertWatchListItem :: NewWatchListItem -> IO ()
insertWatchListItem (NewWatchListItem name (Just airDate) (Just showID)) = do
  conn <- SQLite.open "test.db"
  SQLite.executeNamed conn
    "INSERT INTO watch_list (name, air_date, show_id) VALUES (:name, :airDate, :showID)"
    [":name" := name, ":airDate" := airDate, ":showID" := showID]
insertWatchListItem (NewWatchListItem name Nothing Nothing) = do
  conn <- SQLite.open "test.db"
  SQLite.executeNamed conn
    "INSERT INTO watch_list (name) VALUES (:name)"
    [":name" := name]
-- TODO: Shouldn't be a silent failure
insertWatchListItem _ = return ()

-- refreshWatchList :: IO ()
-- refreshWatchList = do

  -- get all shows from the DB

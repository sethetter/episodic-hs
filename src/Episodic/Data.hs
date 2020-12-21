{-# LANGUAGE QuasiQuotes,OverloadedStrings #-}
module Episodic.Data where
  -- ( loadSchema
  -- , loadSeedData
  -- , refreshWatchList
  -- )

import           Data.Time
import           Data.Text
import           Data.Maybe (fromMaybe, catMaybes)
import           Text.RawString.QQ
import           Database.SQLite.Simple (NamedParam((:=)))
import qualified Database.SQLite.Simple as SQLite
import           Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as JSON
import qualified Episodic.TMDB as TMDB

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

openDB :: String -> IO SQLite.Connection
openDB env = SQLite.open $ env ++ ".db"

loadSchema :: SQLite.Connection -> IO ()
loadSchema conn = SQLite.execute_ conn schema

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
  fromRow = WatchListItem <$> SQLite.field -- id
                          <*> SQLite.field -- name
                          <*> SQLite.field -- air_date
                          <*> SQLite.field -- show_id

instance SQLite.ToRow WatchListItem where
  toRow wli = SQLite.toRow ( wliID wli      -- id
                           , wliName wli    -- name
                           , wliAirDate wli -- air_date
                           , wliShowID wli  -- show_id
                           )

instance JSON.ToJSON WatchListItem where
  toJSON wli = JSON.object [ "id"       .= wliID wli
                           , "name"     .= wliName wli
                           , "air_date" .= wliAirDate wli
                           , "show_id"  .= wliShowID wli
                           ]

getWatchList :: SQLite.Connection -> IO [WatchListItem]
getWatchList conn = do
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
                     <*> v .:? "air_date"
                     <*> v .:? "show_id"

insertWatchListItem :: SQLite.Connection -> NewWatchListItem -> IO ()
insertWatchListItem conn (NewWatchListItem name (Just airDate) (Just showID)) =
  SQLite.executeNamed conn
    "INSERT INTO watch_list (name, air_date, show_id) VALUES (:name, :airDate, :showID)"
    [":name" := name, ":airDate" := airDate, ":showID" := showID]
insertWatchListItem conn (NewWatchListItem name Nothing Nothing) =
  SQLite.executeNamed conn
    "INSERT INTO watch_list (name) VALUES (:name)"
    [":name" := name]
-- TODO: Shouldn't be a silent failure
insertWatchListItem _ _ = return ()


refreshWatchList :: SQLite.Connection -> IO ()
refreshWatchList conn = do
  shows <- getShows conn
  watchList <- getWatchList conn
  episodes <- fmap (\s -> TMDB.getNextEpisode (show'ID s) "") shows
  fmap (saveNextEpisode conn) $ filter epInWatchlist (catMaybes episodes)

epInWatchlist :: [WatchListItem] -> TMDB.Episode -> Bool
epInWatchlist wl (TMDB.Episode _ _ _)

saveNextEpisode :: SQLite.Connection -> TMDB.TVShow -> TMDB.Episode -> IO ()
saveNextEpisode conn (TMDB.TVShow showID name _) (TMDB.Episode num season airDate) =
  -- TODO: if episode is present already, skip it
  insertWatchListItem conn $ NewWatchListItem (name ++ "(S" ++ season ++ "E" ++ num ++ ")") airDate showID


-- Show'
-------------------------------------------------

data Show' = Show'
  { show'ID :: Int
  , show'Name :: Text
  }

instance JSON.FromJSON Show' where
  parseJSON = JSON.withObject "Show'" $ \v ->
    Show' <$> v .: "id"
          <*> v .: "name"

instance JSON.ToJSON Show' where
  toJSON s = JSON.object ["id" .= show'ID s, "name" .= show'Name s]

instance SQLite.ToRow Show' where
  toRow s = SQLite.toRow (show'ID s, show'Name s)

instance SQLite.FromRow Show' where
  fromRow = Show' <$> SQLite.field <*> SQLite.field

getShows :: SQLite.Connection -> IO [Show']
getShows db = SQLite.query_ db "SELECT * FROM shows"

insertShow :: SQLite.Connection -> Show' -> IO ()
insertShow db s =
  SQLite.executeNamed db
    "INSERT INTO shows (id, name) VALUES (:id, :name)"
    [":id" := show'ID s, ":name" := show'Name s]

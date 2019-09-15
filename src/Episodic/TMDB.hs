{-# LANGUAGE OverloadedStrings #-}
module Episodic.TMDB
  ( getShowData
  , Episode
  , TVShow
  ) where

import           Data.Time
import           Data.Text
import           Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import qualified Network.Wreq as Wreq
import           Control.Lens


getShowData :: Int -> String -> IO (Maybe TVShow)
getShowData id' apiKey = do
  r <- Wreq.get $ "https://api.themoviedb.org/3/tv/" ++ show id' ++ "?api_key=" ++ apiKey
  return $ JSON.decode (r ^. Wreq.responseBody)


-- TVShow
------------------------------------------
data TVShow = TVShow
  { _showID          :: Integer
  , _showName        :: Text
  , _showNextEpisode :: Episode
  }
  deriving (Show)

instance JSON.FromJSON TVShow where
  parseJSON = JSON.withObject "TVShow" $ \v ->
    TVShow <$> v .: "id"
           <*> v .: "name"
           <*> v .: "next_episode_to_air"

-- Episode
------------------------------------------
data Episode = Episode
  { _episodeNumber         :: Integer
  , _episodeSeasonNumber   :: Integer
  , _episodeAirDate        :: Day
  }
  deriving (Show)

instance JSON.FromJSON Episode where
  parseJSON = JSON.withObject "Episode" $ \v ->
    Episode <$> v .: "episode_number"
            <*> v .: "season_number"
            <*> v .: "air_date"



module TMDB where ()

import           Data.Text
import qualified Data.Aeson as JSON

data Show = Show
  { showID          :: Integer
  , showName        :: Text
  , showNextEpisode :: Episode
  }

data Episode = Episode
  { episodeNumber  :: Integer
  , seasonNumber   :: Integer
  , episodeAirDate :: Integer
  }

instance JSON.FromJSON Episode where
  parseJSON (JSON.Object v) =
    Episode <$> v .: "episode_number"
            <*> v .: "season_number"
            <*> v .: "air_date"

getShowData :: IO Show
getShowData = do
  -- 

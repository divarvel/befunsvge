module Config where

import           Data.Text       (splitOn, unpack)
import           Options.Generic

data Config
  = Config
  { width    :: Int
  , height   :: Int
  , source   :: Maybe Text
  , fileName :: String
  , maxIter  :: Maybe Natural
  , seed     :: Maybe Int
  -- todo remove
  , step     :: Maybe Int
  }
  deriving (Generic, ParseRecord)

data PerlinConfig
  = PerlinConfig
  { octaves     :: Int
  , scale       :: Double
  , persistence :: Double
  , amp         :: Double
  }
  deriving stock Show

data Source
  = Perlin PerlinConfig
  | Dummy
  deriving stock Show

parsePerlin :: [Text] -> Either String PerlinConfig
parsePerlin [o, s, p, a] = maybeToRight "parseError" $ do
  octaves <- readMaybe $ unpack o
  scale <- readMaybe $ unpack s
  persistence <- readMaybe $ unpack p
  amp <- readMaybe $ unpack a
  pure $ PerlinConfig{..}
parsePerlin _ = Left "Incorrect number of params"

parseSource :: Text -> Either String Source
parseSource input =
  let things = splitOn ":" input
   in traceShowId $ case things of
       "perlin" : values -> Perlin <$> parsePerlin values
       ["dummy"]         -> pure Dummy
       _                 -> Left "Unknown source"

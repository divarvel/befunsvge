module Config where

import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Text           (pack, splitOn, unpack)
import           Options.Applicative
import           Options.Generic

data Config
  = Config
  { width   :: Int
  , height  :: Int
  , source  :: Maybe Source
  , maxIter :: Maybe Natural
  , seed    :: Maybe Int
  }
  deriving (Generic, ParseRecord, FromJSON, ToJSON)

instance ParseField Source where
  readField = eitherReader (parseSource . pack)

data PerlinConfig
  = PerlinConfig
  { octaves     :: Int
  , scale       :: Double
  , persistence :: Double
  , amp         :: Double
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data Source
  = Perlin PerlinConfig
  | Dummy
  deriving (Show, Generic, FromJSON, ToJSON)

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
   in case things of
       "perlin" : values -> Perlin <$> parsePerlin values
       ["dummy"]         -> pure Dummy
       _                 -> Left "Unknown source"

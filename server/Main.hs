{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
module Main where

import           Config
import           Data.Aeson                 (FromJSON, ToJSON, eitherDecode,
                                             encode)
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.Text                  as T
import           Lucid
import           Lucid.Base
import           Network.Wai.Handler.Warp   (runEnv)
import           Relude
import           Servant
import           Servant.HTML.Lucid
import           State
import           Web.FormUrlEncoded         (FromForm)

data RenderingRequestForm
  = RenderingRequestForm
  { width       :: Int
  , height      :: Int
  , maxIter     :: Natural
  , _seed       :: Int
  , octaves     :: Int
  , scale       :: Double
  , persistence :: Double
  , amp         :: Double
  , _program    :: Text
  } deriving (Generic, FromForm)

data RenderingRequest
  = RenderingRequest
  { config  :: Config
  , program :: Board
  }
  deriving (Generic, FromJSON, ToJSON)

instance FromHttpApiData RenderingRequest where
  parseQueryParam v = do
    json <- first toText $ B64.decode (encodeUtf8 v)
    let res = eitherDecode (fromStrict json)
    first toText res

defaultReq :: RenderingRequest
defaultReq = RenderingRequest
  { config = Config
    { width    = 500
    , height   = 500
    , source   = Just $ Perlin $ PerlinConfig
      { octaves     = 5
      , scale       = 0.005
      , persistence = 0.5
      , amp         = 20
      }
    , maxIter = Nothing
    , seed = Nothing
    }
  , program = ">\"d\":55*2*κ@"
  }

type MainPage = Html ()
type API =
       QueryParam "req" RenderingRequest :> Get '[HTML] MainPage
  :<|> ReqBody '[FormUrlEncoded] RenderingRequestForm :> Post '[HTML] NoContent

main :: IO ()
main = runEnv 8080 $ serve @API Proxy server

server :: Server API
server =  getHander :<|> postHandler

postHandler :: RenderingRequestForm -> Handler NoContent
postHandler RenderingRequestForm{..} = do
  let source = Just $ Perlin $ PerlinConfig{..}
  let config = Config{ maxIter = Just (min maxIter 1000000)
                     , seed = mfilter (/= 0) $ Just _seed
                     , ..
                     }
      req = RenderingRequest{ program = readBoard $ T.replace "\r\n" "\n" _program
                            , ..
                            }
  redirectTo req

encodeReq :: RenderingRequest -> ByteString
encodeReq req = B64.encode $ toStrict $ encode req

redirectTo :: RenderingRequest -> Handler a
redirectTo req = do
  let encoded = encodeReq req
  throwError $ err302 { errHeaders = [("Location", "?req=" <> encoded)] }

getHander :: Maybe RenderingRequest -> Handler MainPage
getHander req = do
  when (isNothing req) $ redirectTo defaultReq
  let rr@RenderingRequest{..} = fromMaybe defaultReq req
  r@BState{mode} <- liftIO $ compute config program
  case mode of
    Error e -> throwError $ err400 { errBody = encodeUtf8 e }
    Stopped -> pure $ renderResult rr r
    _       -> throwError $ err500 { errBody = "Execution aborted" }

renderResult :: RenderingRequest -> BState -> MainPage
renderResult RenderingRequest{..} BState{..} = html_ $
  body_ $ do
    let Config{height, width} = config
    svg_ [ height_ (show height)
         , width_ (show width)
         , makeAttribute "fill" "none"
         , makeAttribute "stroke" "black"
         ]
         $ toHtmlRaw output
    p_ $ "Seed " <> show seed
    renderForm config { seed = Just seed } program

renderForm :: Config -> Board -> MainPage
renderForm Config{..} b = do
  let pconf = case source of
         Just (Perlin p) -> Just p
         _               -> Nothing
      _o = (\PerlinConfig{octaves} -> octaves) <$> pconf
      _s = (\PerlinConfig{scale} -> scale) <$> pconf
      _p = (\PerlinConfig{persistence} -> persistence) <$> pconf
      _a = (\PerlinConfig{amp} -> amp) <$> pconf
  form_ [ method_ "POST"
        , action_ "?"
        ] $ do
    textarea_ [ name_ "_program"
              , rows_ (show . max 5 . snd $ size b)
              , cols_ (show . max 20 . fst $ size b)
              ] $ toHtml (renderBoard b)
    br_ []
    p_ "Height (px)"
    input_ [ value_ (show height)
           , name_ "height"
           ]
    br_ []
    p_ "Width (px)"
    input_ [ value_ (show width)
           , name_ "width"
           ]
    br_ []
    p_ "Max iterations"
    input_ [ value_ (maybe "1000" show maxIter)
           , name_ "maxIter"
           ]
    br_ []
    p_ "Perlin seed (0 for random)"
    input_ [ value_ (maybe "0" show seed)
           , name_ "_seed"
           ]
    br_ []
    p_ "Perlin octaves"
    input_ [ value_ (maybe "5" show _o)
           , name_ "octaves"
           ]
    br_ []
    p_ "Perlin scale"
    input_ [ value_ (maybe "0.005" show _s)
           , name_ "scale"
           ]
    br_ []
    p_ "Perlin persistence"
    input_ [ value_ (maybe "0.5" show _p)
           , name_ "persistence"
           ]
    br_ []
    p_ "Perlin amplification"
    input_ [ value_ (maybe "20" show _a)
           , name_ "amp"
           ]
    br_ []
    input_ [ type_ "submit" ]

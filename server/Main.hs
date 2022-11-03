{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
module Main where

import           Codec.Compression.GZip     (compress, decompress)
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

newtype Compressed a = Compressed { getCompressed :: a }

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

instance FromHttpApiData (Compressed RenderingRequest) where
  parseQueryParam v = do
    json <- first toText . fmap (decompress . fromStrict) $ B64.decode (encodeUtf8 v)
    let res = eitherDecode json
    Compressed <$> first toText res

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
       QueryParam "creq" (Compressed RenderingRequest) :>
       QueryParam "req" RenderingRequest :>
       Get '[HTML] MainPage
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

compressAndEncodeReq :: RenderingRequest -> ByteString
compressAndEncodeReq req = B64.encode $ toStrict $ compress $ encode req

redirectTo :: RenderingRequest -> Handler a
redirectTo req = do
  let encoded = compressAndEncodeReq req
  throwError $ err302 { errHeaders = [("Location", "?creq=" <> encoded)] }

getHander :: Maybe (Compressed RenderingRequest) ->
             Maybe RenderingRequest -> Handler MainPage
getHander cReq req' = do
  let req = getCompressed <$> cReq <|> req'
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
    div_ [ style_ "width: 100%; display: flex; justify-content: space-around"] $ do
      svg_ [ height_ (show height)
           , width_ (show width)
           , makeAttribute "fill" "none"
           , makeAttribute "stroke" "black"
           , style_ "border: 1px solid black;"
           ] $ toHtmlRaw svgOutput
      div_ [ style_ "max-width: 50%;" ] $ do
        h2_ "Text output"
        pre_ $ toHtml textOutput
        h2_ "SVG output"
        pre_ $ toHtml svgOutput
    div_ [ style_ "display: flex; justify-content: space-around; width: 100%;"] $ do
      div_ [style_ "max-width: 50%;" ] $ do
        renderForm config { seed = Just seed } program
      div_ renderHelp

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
        , style_ "display: grid; \
                 \grid-template-areas: \"a a\";\
                 \grid-template-columns: 1fr 1fr;\
                 \grid-template-rows: repeat(1fr);"
        ] $ do
    div_ [style_ "grid-area: a;" ] $ do
      h2_ $ do
        "Input "
        input_ [ type_ "submit" ]
      textarea_ [ name_ "_program"
                , rows_ (show . max 5 . snd $ size b)
                , cols_ (show . max 20 . fst $ size b)
                , style_ "width: 100%;"
                ] $ toHtml (renderBoard b)
    div_ $ do
      p_ "Height (px)"
      input_ [ value_ (show height)
             , name_ "height"
             ]
    div_ $ do
      p_ "Width (px)"
      input_ [ value_ (show width)
             , name_ "width"
             ]
    div_ $ do
      p_ "Max iterations"
      input_ [ value_ (maybe "1000" show maxIter)
             , name_ "maxIter"
             ]
    div_ $ do
      p_ "Perlin seed (0 for random)"
      input_ [ value_ (maybe "0" show seed)
             , name_ "_seed"
             ]
      p_ "Perlin octaves"
      input_ [ value_ (maybe "5" show _o)
             , name_ "octaves"
             ]
      p_ "Perlin scale"
      input_ [ value_ (maybe "0.005" show _s)
             , name_ "scale"
             ]
      p_ "Perlin persistence"
      input_ [ value_ (maybe "0.5" show _p)
             , name_ "persistence"
             ]
      p_ "Perlin amplification"
      input_ [ value_ (maybe "20" show _a)
             , name_ "amp"
             ]

renderHelp :: MainPage
renderHelp = do
  h2_ "Befunsvge commands"
  p_ $ do
    h2_ "Regular befunge-93 commands"
    code_ "< > ^ v"
    ": Change flow direction"
    br_ []
    code_ "#"
    ": Jump over next cell"
    br_ []
    code_ "?"
    ": Move in a random direction"
    br_ []
    code_ "+ - * / %"
    ": Pop two values and push the result"
    br_ []
    code_ "`"
    ": Pop a and b, push 1 if b > a, 0 otherwise"
    br_ []
    code_ ":"
    ": Duplicate the stack head"
    br_ []
    code_ "\\"
    ": swap the two elements at the front"
    br_ []
    code_ "$"
    ": pop a stack element"
    br_ []
    code_ "!"
    ": pop a value, push 1 if the value is 0, 0 otherwise"
    br_ []
    code_ "_"
    ": pop a value, go right if the value is 0, go left otherwise"
    br_ []
    code_ "|"
    ": pop a value, go down if the value is 0, go up otherwise"
    br_ []
    code_ "@"
    ": stop execution"
    br_ []
    code_ "\""
    ": toggle string mode (push char codepoints between to quotes)"
    br_ []
    code_ "p"
    ": pop coords and value, set the corresponding cell to the value"
    br_ []
    code_ "g"
    ": pop coords, push the corresponding cell value"
    br_ []
    code_ "."
    ": pop a value, display the corresponding number"
    br_ []
    code_ "$"
    ": pop a value, display the corresponding char (codepoint)"
    h2_ "SVG extensions"
    code_ "κ ρ ε"
    ": pop values, output the corresponding SVG tag "
    fold ["(", code_ "circle", ", ", code_ "rect", ", ", code_ "ellipse", ")"]
    br_ []
    code_ "A a C c S s Q q M m L l T t H h Z z"
    ": pop values, push the corresponding directive to the current path buffer"
    br_ []
    code_ "W w"
    fold [ ": pop values, push the corresponding directive ("
         , code_ "V v"
         , ") to the current path buffer"
         ]
    br_ []
    code_ "π"
    fold [ ": flush the path buffer, output the corresponding "
         , code_ "path_"
         , " directive"
         ]
    h2_ "External input extension"
    code_ "i"
    ": pop x y z coordinates and push f(x,y,z). For now, f is just Perlin noise, with parameters below."

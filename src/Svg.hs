{-# LANGUAGE NoImplicitPrelude #-}
module Svg where

import           Relude

import           Config

renderDoc :: Config -> Text -> Text
renderDoc Config{..} body =
  fold
    [ "<body>"
    , "<svg width=\"" <> show width <> "px\" height=\"" <> show height <> "px\" fill=\"none\" stroke=\"black\">"
    , body
    , "</svg>"
    , "</body>"
    ]

{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Config
import           Perlin
import           Relude
import           State

import qualified Data.Text.IO       as T
import           Options.Generic
import           System.Environment

main :: IO ()
main = do
  c@Config{..} <- getRecord "Config"
  pgm <- T.getContents
  render c $ readBoard pgm

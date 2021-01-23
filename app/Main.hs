module Main where

import           Config
import           Perlin
import           State

import qualified Data.Text          as T
import           Options.Generic
import           System.Environment

main :: IO ()
main = do
  c@Config{..} <- getRecord "Config"
  pgm <- T.pack <$> readFile fileName
  render c $ readBoard pgm

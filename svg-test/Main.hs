module Main where

import           Config
import           Options.Generic
import           Perlin
import           System.Environment (getArgs)

main :: IO ()
main = do
  c@Config{..} <- getRecord "Config"
  render' 10 c

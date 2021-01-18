module Main where

import           State

import qualified Data.Text          as T
import           System.Environment

main :: IO ()
main = do
  (fileName, maxIter) <- getArgs <&> \case
    [fileName] -> (fileName, Nothing)
    [fileName, m] -> (fileName, readMaybe m)
  pgm <- T.pack <$> readFile fileName
  render maxIter $ readBoard pgm

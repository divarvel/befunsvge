module Perlin where

import qualified Data.Text            as T
import           GHC.Float
import           Numeric.Noise.Perlin
import           System.Random

import           Config
import           Svg

getPerlin :: Config -> IO (Perlin, Double)
getPerlin Config{..} = do
  seed' <- maybe randomIO pure seed
  let (Perlin PerlinConfig{..}) = either (error . toText) id $ parseSource $ maybe undefined id source
      noise = perlin seed' octaves scale persistence
  pure (noise, amp)

test :: Int -> Config -> IO Text
test step c = do
  (noise,amp) <- getPerlin c
  let lns = lines' step amp noise
  pure $ foldMap line2Path lns

line :: Int -> Double -> Perlin -> Int -> [(Int, Int)]
line step amp noise rank =
  let deltaAt x y = double2Int $ (amp*) $ noiseValue noise (int2Double x, int2Double y, 0)
      mapPoint y = (rank + deltaAt rank y, y)
   in mapPoint <$> [0,step..1000]

lines' :: Int -> Double -> Perlin -> [[(Int, Int)]]
lines' step amp noise =
  line step amp noise <$> [0,step..1000]

line2Path :: [(Int,Int)] -> Text
line2Path (p:ps) =
  let s c (x,y) = c <> " " <> show x <> " " <> show y <> " "
      d = s "M" p <> foldMap (s "L") ps
   in "<path d=\"" <> d <> "\" />"

render' :: Int -> Config -> IO ()
render' step c = do
  p <- test step c
  putStrLn . toString $ renderDoc c p

{-# LANGUAGE NoImplicitPrelude #-}
module State where

import           Data.Aeson           (FromJSON (..), ToJSON (..))
import           Data.Char            (isDigit)
import           Data.Foldable        (maximum)
import           Data.Map.Strict      (findWithDefault, insert, (!?))
import           Data.Set             (member, singleton)
import qualified Data.Text            as T
--import           Data.Text.Encoding.Base64.URL (decodeBase64, encodeBase64)
import           GHC.Float
import           Numeric.Noise.Perlin
import           Relude
import           System.IO            (hPutStrLn)
import           System.Random        (StdGen, mkStdGen, random, randomIO)

import           Config

type Position = (Natural, Natural)

type Delta = (Int, Int)

type Pointer = (Delta, Position)

data Mode = Normal | StringInput | Stopped | Error Text
  deriving stock (Eq, Ord, Show)

data Board
  = Board
  { size :: Position
  , grid :: Map Position Char
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsString Board where
  fromString = readBoard . T.pack

{-
instance FromJSON Board where
  parseJSON = withText "board" (either (fail . toString) (pure . readBoard) . decodeBase64)

instance ToJSON Board where
  toJSON Board{..} = String $ encodeBase64 $ renderGrid grid size
  -}

data BState
  = BState
  { stack       :: [Int]
  , currentPath :: Text
  , svgOutput   :: Text
  , textOutput  :: Text
  , board       :: Board
  , pointer     :: Pointer
  , iterations  :: Natural
  , mode        :: Mode
  , randGen     :: StdGen
  , input       :: (Int, Int, Int) -> Int
  , seed        :: Int
  }

move :: Delta -> BState -> BState
move d s@BState{pointer=(_, p)} = s { pointer = (d, p) }

jump :: BState -> BState
jump s@BState{pointer=(d,p)}=
  let d' = case d of
             (0,  1) -> (0,2)
             (0, -1) -> (0,-2)
             (1,  0) -> (2,0)
             (-1, 0) -> (-2,0)
             _       -> d
   in s { pointer = (d', p) }

stop :: BState -> BState
stop s = s { mode = Stopped }

bin :: (Int -> Int -> Int) -> BState -> BState
bin op s@BState{..} = case stack of
  (a:b:rest) -> s { stack = b `op` a : rest }
  _ -> s { mode = Error "Binary operation requires two elements in the stack" }

push :: Int -> BState -> BState
push int s@BState{stack} = s { stack = int : stack }

-- should this error on empty stacks?
pop :: BState -> BState
pop s@BState{stack} = s { stack = drop 1 stack }

-- should this error on empty stacks?
dup :: BState -> BState
dup s@BState{stack} = case stack of
  x:xs -> s { stack = x:x:xs }
  _    -> stackErr 1 s

-- should this error on empty stacks?
sswap :: BState -> BState
sswap s@BState{stack} = case stack of
  x:y:xs -> s { stack = y:x:xs }
  _      -> stackErr 2 s

cond :: Delta -> Delta
     -> BState -> BState
cond ifNZ ifZ s@BState{stack} = case stack of
  0:xs -> move ifZ $ s { stack = xs }
  _:xs -> move ifNZ $ s { stack = xs }
  _    -> stackErr 1 s

comp :: BState -> BState
comp s@BState{stack} = case stack of
  a:b:xs | b > a     -> s { stack = 1 : xs }
         | otherwise -> s { stack = 0 : xs }
  _ -> stackErr 2 s

nnot :: BState -> BState
nnot s@BState{stack} = case stack of
  0:xs -> s { stack = 1 : xs }
  _:xs -> s { stack = 0 : xs }
  _    -> stackErr 1 s

randomMove :: BState -> BState
randomMove s@BState{..} =
  let (b1, g) = random randGen
      (b2, g') = random g
      (_, pos) = pointer
      newDelta = case (b1, b2) of
        (False, False) -> (0,1)
        (False, True)  -> (0,-1)
        (True,  False) -> (1,0)
        (True,  True)  -> (-1,0)
   in s { randGen = g'
        , pointer = (newDelta, pos)
        }

inRange :: Board
        -> Int -> Int
        -> Bool
inRange Board{size=(mx,my)} x y =
     x >= 0 && toInteger x < toInteger mx
  && y >= 0 && toInteger y < toInteger my

putCell :: BState -> BState
putCell s@BState{..} = case stack of
  y:x:v:xs | inRange board x y ->
              s { stack = xs
                , board = board { grid = insert (toNatural x, toNatural y) (chr v) (grid board) }
                }
           | otherwise -> err "Cell out of range" s
  _ -> stackErr 3 s

getCell :: BState -> BState
getCell s@BState{..} = case stack of
  y:x:xs | inRange board x y ->
             let v = findWithDefault ' ' (toNatural x, toNatural y) (grid board)
             in s { stack = ord v : xs }
         | otherwise -> err "Cell out of range" s
  _ -> stackErr 2 s

toggleStringMode :: BState -> BState
toggleStringMode s@BState{mode} = case mode of
  Normal      -> s { mode = StringInput }
  StringInput -> s { mode = Normal }
  _           -> s

popChar :: BState -> BState
popChar s@BState{textOutput,stack} = case stack of
  c:xs -> s { textOutput = textOutput <> one (chr c)
            , stack = xs
            }
  _ -> stackErr 1 s

popNumber :: BState -> BState
popNumber s@BState{textOutput,stack} = case stack of
  n:xs -> s { textOutput = textOutput <> show n
            , stack = xs
            }
  _ -> stackErr 1 s

err :: Text -> BState -> BState
err e s = s { mode = Error e }

stackErr :: Int -> BState -> BState
stackErr n = err $ "Expected stack to have at least " <> show n <> " element"

handle :: Char -> BState -> BState
handle c = case c of
  -- direction
  '^'                 -> move (0, -1)
  'v'                 -> move (0, 1)
  '<'                 -> move (-1,0)
  '>'                 -> move (1, 0)
  '#'                 -> jump
  '?'                 -> randomMove
  -- arithmetic
  '+'                 -> bin (+)
  '-'                 -> bin (-)
  '*'                 -> bin (*)
  '/'                 -> bin div
  '%'                 -> bin mod
  '`'                 -> comp
  -- stack manipulation
  ':'                 -> dup
  '\\'                -> sswap
  '$'                 -> pop
  -- branching
  '!'                 -> nnot
  '_'                 -> cond (-1,0) (1,0)
  '|'                 -> cond (0,-1) (0,1)
  '@'                 -> stop
  -- string mode
  '"'                 -> toggleStringMode
  -- reflection
  'p'                 -> putCell
  'g'                 -> getCell
  -- output
  '.'                 -> popNumber
  ','                 -> popChar
  -- SVG
  'κ'                 -> popTag "circle" ["r", "cx", "cy"]
  'ρ'                 -> popTag "rect" ["width", "height", "x", "y"]
  'ε'                 -> popTag "ellipse" ["rx", "ry", "cx", "cy"]
  -- SVG - path
  n | n `elem` ("Aa" :: String) -> popPathCommand (one n) 7
  n | n `elem` ("Cc" :: String) -> popPathCommand (one n) 6
  n | n `elem` ("SsQq" :: String) -> popPathCommand (one n) 4
  n | n `elem` ("MmLlTt" :: String) -> popPathCommand (one n) 2
  n | n `elem` ("Hh" :: String)   -> popPathCommand (one n) 1
  n | n `elem` ("Zz" :: String)   -> popPathCommand (one n) 0
  -- v is already taken by the direction operator
  'W'   -> popPathCommand "V" 1
  'w'   -> popPathCommand "v" 1
  'π'                 -> flushPath
  -- input
  'i'   -> getInput
  n | isDigit n       -> push (fromMaybe 0 $ readMaybe [n])
  _                   -> id

getInput :: BState -> BState
getInput s@BState{stack,input} = case stack of
  z:y:x:xs -> s { stack = input (x,y,z) : xs }
  _        -> stackErr 3 s

run :: Maybe Natural
    -> ((Int, Int, Int) -> Int)
    -> (Int, StdGen)
    -> Board
    -> BState
run maxIter input (seed, randGen) board@Board{grid} =
  let position = (0,0)
      delta = (1,0)
      pointer = (delta, position)
      iterations = 0
      mode = Normal
      stack = []
      currentPath = ""
      textOutput = ""
      svgOutput = ""
      bstate = BState{ .. }
      c = grid !? position
   in runStep (fromMaybe 1000000 maxIter) c bstate

stateData :: BState -> ([Int], Pointer)
stateData BState{..} = (stack, pointer)

renderState :: Maybe Char -> BState -> Text
renderState c BState{..} =
  let pChar = case fst pointer of
        (1, 0)  -> '→'
        (2, 0)  -> '⇨'
        (-1, 0) -> '←'
        (-2, 0) -> '⇦'
        (0, 1)  -> '↓'
        (0, 2)  -> '⇩'
        (0, -1) -> '↑'
        (0, -2) -> '⇧'
        _       -> '?'
      gridWithPointer = insert (snd pointer) pChar (grid board)
    in unlines
         [ "------------------------------------------"
         , "Iterations: " <> show iterations
         , "Instruction: " <> maybe "∅" one c
         , "Stack: " <> show stack
         , "Path: " <> show currentPath
         , "Mode: " <> show mode
         , "Text Output: " <> textOutput
         , "SVG Output: " <> svgOutput
         , "Pointer: " <> show pointer
         , "Board: "
         , renderGrid (grid board) (size board)
         , renderGrid gridWithPointer (size board)
         ]

renderBoard :: Board
            -> Text
renderBoard Board{..} = renderGrid grid size

renderGrid :: Map Position Char
           -> (Natural, Natural)
           -> Text
renderGrid grid (mx, my) =
  let getAt y x = findWithDefault ' ' (x,y) grid
      renderRow y = toText $ getAt y <$> [0..mx-1]
   in unlines $ renderRow <$> [0..my-1]

incIterations :: Natural -> BState -> BState
incIterations maxIter s@BState{iterations} =
  if iterations >= maxIter
  then err "Iterations maxed out" s
  else s { iterations = iterations + 1 }

runStep :: Natural -> Maybe Char -> BState -> BState
runStep maxIter c s =
  let go handled' =
         let handled = handled' -- trace (T.unpack $ renderState c handled') handled'
             iterated@BState{mode, pointer, board} = incIterations maxIter handled
             newPos  = next pointer board mode
             continue (p, c') = runStep maxIter (Just c') (iterated { pointer = pointer $> p })
          in maybe (err "Infinite loop" s) continue newPos
  in case (mode s, c) of
       (Normal, _)             -> go $ maybe s (`handle` s) c
       (StringInput, Just '"') -> go $ maybe s (`handle` s) c
       (StringInput, _)        -> go $ push (maybe 32 ord c) s
       _                       -> s

-- returns `Nothing` if we hit an infinite loop
next :: Pointer -> Board -> Mode -> Maybe (Position, Char)
next p = next' (singleton p) p

next' :: Set Pointer -> Pointer -> Board -> Mode -> Maybe (Position, Char)
next' _ p Board{..} StringInput =
  let (x', y') = uncurry nextPos p size
   in Just ((x', y'), findWithDefault ' ' (x', y') grid)
next' visited p b@Board{..} m =
  let (x', y') = uncurry nextPos p size
      p' = p $> (x',y')
   in if p' `member` visited
      then Nothing
      else case grid !? (x',y') of
            Nothing -> next' (visited <> singleton p') p' b m
            Just c  -> Just ((x',y'), c)

nextPos :: Delta -> Position -> Position -> Position
nextPos (dx, dy) (x, y) (mx, my) =
  let (ix, iy) = (toInteger x, toInteger y)
      (x',y') = (ix + toInteger dx, iy + toInteger dy)
      clamp' m v  | v < 0     = m - 1
                  | v >= toInteger m    = 0
                  | otherwise = fromInteger v
   in ( clamp' mx x'
      , clamp' my y'
      )

popTag :: Text
       -> [Text]
       -> BState -> BState
popTag name attrs s@BState{stack, svgOutput} =
  let pairs = fmap show <$> zip attrs stack
   in if length pairs == length attrs
      then s { stack = drop (length pairs) stack
             , svgOutput = svgOutput <> outputTag name pairs
             }
      else stackErr (length attrs) s

outputTag :: Text
          -> [(Text, Text)]
          -> Text
outputTag name attrs =
  let toPair (k,v) = k <> "=\"" <> v <> "\" "
      attrs' = foldMap toPair attrs
   in "<" <> name <> " " <> attrs' <> "/>"

popPathCommand :: Text -> Int
               -> BState -> BState
popPathCommand name argc s@BState{stack,currentPath} =
  if argc > length stack
  then stackErr argc s
  else s { currentPath = currentPath <> " " <> name <> " " <> T.intercalate " " (show <$> take argc stack)
         , stack = drop argc stack
         }

flushPath :: BState -> BState
flushPath s@BState{currentPath,svgOutput} =
  let tag = outputTag "path" [("d", currentPath)]
   in s { svgOutput = svgOutput <> tag
        , currentPath = ""
        }

dummyBoard' :: Board
dummyBoard' =
  ">02`!#v_@\n\
  \      8\n\
  \      @"
  {-
  "v v        <\n\
  \            \n\
  \        >2+^\n\
  \>0>:25*`|   \n\
  \        @   "
  -}
toNatural :: Integral a => a -> Natural
toNatural = fromInteger . toInteger

getInputFunc :: Int -> Source -> ((Int, Int, Int) -> Int)
getInputFunc seed (Perlin PerlinConfig{..}) =
  let noise = perlin seed octaves scale persistence
      rescale = double2Int . (* amp)
   in \(x,y,z) ->
       let p = (int2Double x, int2Double y, int2Double z)
        in rescale $ noiseValue noise p
getInputFunc _ Dummy = const 0

compute :: Config
        -> Board
        -> IO BState
compute Config{..} b = do
  let source' = fromMaybe Dummy source
  seed' <- maybe randomIO pure seed
  let getInput' = getInputFunc seed' source'
  let randGen = mkStdGen seed'
  pure $ run maxIter getInput' (seed', randGen) b

render :: Config
       -> Board
       -> IO ()
render c@Config{..} b = do
  result@BState{textOutput, svgOutput} <- compute c b
  hPutStrLn stderr $ toString $ renderState Nothing result
  hPutStrLn stderr $ toString textOutput
  putStrLn "<body>"
  putStrLn $ "<svg width=\"" <> show width <> "px\" height=\"" <> show height <> "px\" fill=\"none\" stroke=\"black\">"
  putStrLn (T.unpack svgOutput)
  putStrLn "</svg>"
  putStrLn "</body>"

readBoard :: Text -> Board
readBoard input =
  let rows = lines input
      maxWidth = maximum $ T.length <$> rows
      zipWithIndex = zip [0..]
      size = (toNatural maxWidth, toNatural $ length rows)
      rowToPoints y =
        let go (pts, x) c =
              if c == ' '
              then (pts, x + 1)
              else (pts <> one ((x,y), c), x + 1)
         in fst . T.foldl' go (mempty, 0)
      grid = foldMap (uncurry rowToPoints) $ zipWithIndex rows
   in Board{..}

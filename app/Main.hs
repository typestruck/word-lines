{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import           Miso               (App, styles, mount, Effect, View, defaultEvents, MisoString)
import qualified Miso               as M
import qualified Miso.Html.Element  as HE
import qualified Miso.Html.Property as HP
import qualified Miso.Html.Event as HP
import qualified Miso.State         as MS
import Letters as L
import qualified System.Random as MR
import System.Random (StdGen)
import Control.Monad.RWS (MonadState)
import       Data.Text (Text)
import Styles (styleSheet)
import qualified Data.HashMap.Strict as DM
import qualified Data.HashSet as DS
import Data.HashSet (HashSet)
import qualified Data.List as DL
import Prelude hiding (words)
import qualified Data.Text as DT
import qualified Miso.String as MSS
import qualified Data.Text.IO as DTI

default (MisoString)

data Action =
  NewGame
  | SelectTile Tile
  | PlaceTile (Maybe Tile) Int
  | SubmitWords
  deriving (Show, Eq)

data Tile = Tile { id :: Int, letter :: Int } deriving (Show, Eq)

data Player = Player { tiles :: [Tile], score :: Int} deriving (Show, Eq)

data Model = Model
  { board :: [Tile]
  , home  :: Player
  , away  :: Player
  , selected :: Maybe Tile
  , generator :: StdGen
  , dictionary :: HashSet Text
  }

instance Eq Model where
  m == n = m.board == n.board && m.home == n.home && m.away == n.away && m.selected == n.selected

size :: Int
size = 13

#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif

main :: IO ()
#ifdef INTERACTIVE
main = do
  generator <- MR.newStdGen
  dictionary <- readDictionary
  M.live defaultEvents $ app dictionary generator
#else
main = do
  generator <- MR.newStdGen
  dictionary <- readDictionary
  M.startApp defaultEvents $ app dictionary generator
#endif

readDictionary :: IO (HashSet Text)
readDictionary = do
  --contents <- DTI.readFile "dict"
  pure . DS.fromList $ DT.words "contents"

app :: HashSet Text -> StdGen -> App Model Action
app dictionary generator = (M.component initialModel updateModel viewModel) { mount = Just NewGame, styles = [ styleSheet ] }
  where
  initialModel = Model { board = emptyBoard, home = barePlayer, away = barePlayer, generator = generator, dictionary = dictionary, selected = Nothing }

barePlayer :: Player
barePlayer = Player { tiles = [], score = 0 }

emptyBoard :: [Tile]
emptyBoard = zipWith Tile [1..size *size] $ replicate (size * size) 0

updateModel :: Action -> Effect parent Model Action
updateModel =
  \case
    NewGame -> newGame
    SelectTile t -> selectTile t
    PlaceTile t i -> placeTile t i
    SubmitWords -> submitWords

newGame :: Effect parent Model Action
newGame  = do
  homeLetters <- produceTiles
  awayLetters <- produceTiles
  vowel <- initialVowel
  MS.modify
    $ \m ->
        m { board = replaceAt 84 vowel emptyBoard
        , home = m.home { tiles = homeLetters }
        , away = m.away { tiles =  awayLetters }
        }
  where
  produceTiles = zipWith Tile [1..size] <$> randomLetters size
  initialVowel = do
    model <- MS.get
    let (d :: Float, nextGenerator) = MR.random model.generator
    MS.modify $ \m -> m { generator = nextGenerator }
    return $ if  d >= 0.0 && d < 0.25 then
        letterE
    else if d >= 0.25 && d < 0.50 then
        letterI
    else if d >= 0.50 && d < 0.75 then
        letterA
    else
        letterO

replaceAt :: Int -> Int -> [Tile] -> [Tile]
replaceAt i letter = map f
  where
  f t
    | t.id == i = Tile {id = i, letter}
    | otherwise = t

selectTile :: Tile -> Effect parent Model Action
selectTile t = do
  MS.modify $ \m -> m { selected = Just t }

placeTile :: Maybe Tile -> Int -> Effect parent Model Action
placeTile t i =  case t of
  Nothing -> pure ()
  Just tile -> do
    MS.modify $ \m -> m { selected = Nothing, board = replaceAt i tile.letter m.board, home = m.home { tiles =  filter (tile /= ) m.home.tiles } }

submitWords :: Effect parent Model Action
submitWords = do
  model <- MS.get
  let (good, invalid) = checkWords model.dictionary model.board
  --if null invalid then
  pure ()

checkWords :: HashSet Text -> [Tile] -> ([[Tile]], [[Tile]])
checkWords dictionary board = check words [] []
  where
  words = collectWords rows <> collectWords columns
  rows = board
  columns = reorient board . DM.fromList . zip [0..size -1] $ replicate size [] --size - 1 because size % size = 0

  reorient [] running = concat $ DM.elems running
  reorient (t : iles) running = reorient iles (DM.adjust (++ [t]) (t.id `mod` size) running)

  collectWords = DL.groupBy (\t u -> t.letter /= 0 && u.letter /= 0 && t.id `mod` size > 0)

  check :: [[Tile]] -> [[Tile]] -> [[Tile]] -> ([[Tile]], [[Tile]])
  check [] good invalid = (good, invalid)
  check (w : ords) good invalid = if DS.member (DT.concat $ map (MSS.fromMisoString . L.displayLetter . letter) w) dictionary then check ords (w : good) invalid else check ords good (w : invalid)

randomLetters :: MonadState Model m => Int -> m [Int]
randomLetters howMany = do
  model <- MS.get
  let (letters, generator) = go howMany ([], model.generator)
  MS.modify $ \m -> m { generator = generator }
  return letters
  where go 0 t = t
        go n (letters, gen) =  let (l, nextGen) = MR.random gen in go (n - 1) (pickLetter l : letters, nextGen)

        pickLetter :: Float -> Int
        pickLetter d
          | d >= 0.538979491 && d <= 0.644002294 = letterE
          | d >= 0.267441153 && d <= 0.357286225 = letterI
          | d >= 0.785527013 && d <= 0.873785630 = letterA
          | d >= 0.105176925 && d <= 0.181368894 = letterO
          | d >= 0.418384245 && d <= 0.490188708 = letterR
          | d >= 0.682697737 && d <= 0.753512962 = letterN
          | d >= 0.903911257 && d <= 0.971676778 = letterT
          | d >= 0.023092087 && d <= 0.084533740 = letterS
          | d >= 0.198858882 && d <= 0.256828792 = letterL
          | d >= 0.366164283 && d <= 0.411389683 = letterC
          | d >= 0.496249139 && d <= 0.535292247 = letterU
          | d >= 0.647066784 && d <= 0.681059310 = letterP
          | d >= 0.754715013 && d <= 0.785527012 = letterM
          | d >= 0.873785631 && d <= 0.903911256 = letterD
          | d >= 0.971676779 && d <= 1.000000001 = letterH
          | d >= 0.000000000 && d <= 0.023092086 = letterY
          | d >= 0.084533741 && d <= 0.105176924 = letterG
          | d >= 0.181368895 && d <= 0.198858881 = letterB
          | d >= 0.256828793 && d <= 0.267441152 = letterF
          | d >= 0.357286226 && d <= 0.366164282 = letterV
          | d >= 0.411389684 && d <= 0.418384244 = letterK
          | d >= 0.490188709 && d <= 0.496249138 = letterW
          | d >= 0.535292248 && d <= 0.538979490 = letterZ
          | d >= 0.644002295 && d <= 0.647066783 = letterX
          | d >= 0.681059311 && d <= 0.682697736 = letterQ
          | d >= 0.753512963 && d <= 0.75471501 = letterJ
          | otherwise = error "missing letter range"

viewModel :: Model -> View Model Action
viewModel m = HE.main_ [] [
    HE.div_ [HP.className "left-side"] [
      HE.div_ [HP.className "board"] $ map (\t -> makeTile (PlaceTile m.selected t.id) t) m.board,
      HE.div_ [HP.className "home-tiles"] $ map (\t -> makeTile (SelectTile t) t) m.home.tiles
    ],
    HE.div_ [HP.className "right-side"] [
      HE.div_ [HP.className "submit-button"] [
        HE.button_ [HP.className "submit"] [M.text "Submit"]
      ],
      HE.button_ [HP.className "submit"] [M.text "Pass"]
    ]
  ]
  where
  makeTile action t = HE.div_ [HP.className "tile", HP.onClick action] [M.text $ if t.letter == 0 then "" else L.displayLetter t.letter ]
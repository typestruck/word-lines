{-# LANGUAGE CPP #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Monad qualified as CM
import Control.Monad.RWS (MonadState)
import Data.Bifunctor qualified as DB
import Data.HashMap.Strict qualified as DM
import Data.HashSet (HashSet)
import Data.HashSet qualified as DS
import Data.List qualified as DL
import Data.Text (Text)
import Debug.Trace (traceShow)
import Dictionary (englishDictionary)
import Letters as L
import Miso (App, Effect, MisoString, View, defaultEvents, mount, styles)
import Miso qualified as M
import Miso.Html.Element qualified as HE
import Miso.Html.Event qualified as HP
import Miso.Html.Property qualified as HP
import Miso.State qualified as MS
import Miso.String qualified as MSS
import Styles (styleSheet)
import System.Random (StdGen)
import System.Random qualified as MR
import Prelude hiding (words)

default (MisoString)

data Action
    = NewGame
    | SelectTile Tile
    | ToggleTile (Maybe Tile) Int
    deriving (Show, Eq)

data Status = Valid | Invalid deriving (Show, Eq)

data Tile = Tile {id ∷ Int, letter ∷ Int, status ∷ Status} deriving (Show, Eq)

data Player = Player {tiles ∷ [Tile], score ∷ Int} deriving (Show, Eq)

data Model = Model
    { board ∷ [Tile]
    , home ∷ Player
    , away ∷ Player
    , selected ∷ Maybe Tile
    , generator ∷ StdGen
    , dictionary ∷ HashSet Text
    }

instance Eq Model where
    m == n = m.board == n.board && m.home == n.home && m.away == n.away && m.selected == n.selected

size ∷ Int
size = 13

startingTiles ∷ Int
startingTiles = 8

#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif

main ∷ IO ()
#ifdef INTERACTIVE
main = do
    generator ← MR.newStdGen
    M.reload defaultEvents $ app englishDictionary generator
#else
main = do
    generator ← MR.newStdGen
    M.startApp defaultEvents $ app englishDictionary generator
#endif

app ∷ HashSet Text → StdGen → App Model Action
app dictionary generator = (M.component initialModel updateModel viewModel){mount = Just NewGame, styles = [styleSheet]}
  where
    initialModel = Model{board = emptyBoard, home = barePlayer, away = barePlayer, generator = generator, dictionary = dictionary, selected = Nothing}

barePlayer ∷ Player
barePlayer = Player{tiles = [], score = 0}

bareTile ∷ Int → Int → Tile
bareTile i l = Tile{id = i, letter = l, status = Invalid}

emptyBoard ∷ [Tile]
emptyBoard = zipWith bareTile [1 .. size * size] $ replicate (size * size) 0

updateModel ∷ Action → Effect parent Model Action
updateModel =
    \case
        NewGame → newGame
        SelectTile t → selectTile t
        ToggleTile t i → toggleTile t i

newGame ∷ Effect parent Model Action
newGame = do
    homeLetters ← produceTiles
    awayLetters ← produceTiles
    vowel ← initialVowel
    MS.modify $
        \m →
            m
                { board = replaceAt 85 vowel emptyBoard
                , home = m.home{tiles = homeLetters}
                , away = m.away{tiles = awayLetters}
                }
  where
    produceTiles = zipWith bareTile [1 .. startingTiles] <$> randomLetters startingTiles
    initialVowel = do
        model ← MS.get
        let (d ∷ Float, nextGenerator) = MR.random model.generator
        MS.modify $ \m → m{generator = nextGenerator}
        return $
            if d >= 0.0 && d < 0.25 then
                letterE
            else
                if d >= 0.25 && d < 0.50 then
                    letterI
                else
                    if d >= 0.50 && d < 0.75 then
                        letterA
                    else
                        letterO

replaceAt ∷ Int → Int → [Tile] → [Tile]
replaceAt i letter = map f
  where
    f t
        | t.id == i = Tile{id = i, letter, status = Invalid}
        | otherwise = t

selectTile ∷ Tile → Effect parent Model Action
selectTile t = MS.modify $ \m → m{selected = Just t}

toggleTile ∷ Maybe Tile → Int → Effect parent Model Action
toggleTile t i = case t of
    Nothing → do
        model ← MS.get
        let tile = tileAt model.board i
        CM.when (not (isEmptyTile tile) && length model.home.tiles < startingTiles)
            . MS.modify
            $ \m →
                m
                    { board = replaceAt i 0 m.board
                    , home = m.home{tiles = bareTile (length model.home.tiles + 1) tile.letter : m.home.tiles}
                    }
    Just tile → do
        model ← MS.get
        CM.when (canPlaceTile model.board) $ do
            let existing = (tileAt model.board i).letter
                updatedBoard = checkBoard model.dictionary $ replaceAt i tile.letter model.board
                updatedHome = filter (tile /=) model.home.tiles
            newLetters ← randomLetters $ startingTiles - length updatedHome
            MS.modify $ \m →
                m
                    { selected = Nothing
                    , board = updatedBoard
                    , home =
                        m.home
                            { tiles =
                                if existing > 0 then
                                    bareTile tile.id existing : updatedHome
                                else
                                    if (tileAt updatedBoard i).status == Valid then
                                        map (bareTile tile.id) newLetters <> updatedHome
                                    else
                                        updatedHome
                            }
                    }
  where
    canPlaceTile = not . all isEmptyTile . filter (\tl → tl.id == i + 1 || tl.id == i - 1 || tl.id == i - size || tl.id == i + size)

checkBoard ∷ HashSet Text → [Tile] → [Tile]
checkBoard dictionary board = map check board
  where
    (valid, invalid) = DB.bimap DS.fromList DS.fromList $ checkWords dictionary board
    check t
        | DS.member t.id valid = t{status = Valid}
        | DS.member t.id invalid = t{status = Invalid}
        | otherwise = t

checkWords ∷ HashSet Text → [Tile] → ([Int], [Int])
checkWords dictionary board = check words [] $ map (\[t] → t.id) straggles
  where
    (words, straggles) =
        let r = collectWords rows [] []
            c = collectWords columns [] []
         in (filter ((1 <) . length) (r <> c), filter ((1 ==) . length) r `DL.intersect` filter ((1 ==) . length) c)

    rows = board
    columns = reorient board . DM.fromList . zip [0 .. size - 1] $ replicate size [] -- size - 1 because size % size = 0
    reorient [] running = concat $ DM.elems running
    reorient (t : iles) running = reorient iles (DM.adjust (++ [t]) (t.id `mod` size) running)

    collectWords [] final running = if null running then final else running : final
    collectWords (f : rom) final running =
        if isEmptyTile f || f.id `mod` size == 0 then
            if null running then
                collectWords rom final running
            else
                collectWords rom (running : final) []
        else
            collectWords rom final (running <> [f])

    check ∷ [[Tile]] → [Int] → [Int] → ([Int], [Int])
    check [] valid invalid = (valid, invalid)
    check (w : ords) valid invalid = if DS.member (MSS.fromMisoString . MSS.concat $ map (L.displayLetter . letter) w) dictionary then check ords (add w valid) invalid else check ords valid (add w invalid)

    add w list = map (\t → t.id) w <> list

tileAt ∷ [Tile] → Int → Tile
tileAt board i = board !! (i - 1)

isEmptyTile ∷ Tile → Bool
isEmptyTile t = t.letter == 0

randomLetters ∷ (MonadState Model m) ⇒ Int → m [Int]
randomLetters howMany = do
    model ← MS.get
    let (letters, generator) = go howMany ([], model.generator)
    MS.modify $ \m → m{generator = generator}
    return letters
  where
    go 0 t = t
    go n (letters, gen) = let (l, nextGen) = MR.random gen in go (n - 1) (pickLetter l : letters, nextGen)

    pickLetter ∷ Float → Int
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

viewModel ∷ Model → View Model Action
viewModel m =
    HE.main_
        []
        [ HE.div_
            [HP.className "left-side"]
            [ HE.div_ [HP.className "board"] $ map (\t → makeTile (ToggleTile m.selected t.id) t) m.board
            , HE.div_ [HP.className "home-tiles"] $ map (\t → makeTile (SelectTile t) t) $ DL.sortBy alpha m.home.tiles
            ]
        , HE.div_
            [HP.className "right-side"]
            [ HE.div_
                [HP.className "submit-button"]
                [ HE.button_ [HP.className "submit"] [M.text "End game"]
                ]
            , HE.button_ [HP.className "submit"] [M.text "Replace"]
            ]
        ]
  where
    alpha t u = compare t.letter u.letter

    makeTile action t = HE.div_ [HP.classList_ [("tile", True), ("valid", t.status == Valid)], HP.onClick action] [M.text $ if isEmptyTile t then "" else L.displayLetter t.letter]

{-# LANGUAGE CPP #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import Control.Monad qualified as CM
import Control.Monad.RWS (MonadState)
import Data.Bifunctor qualified as DB
import Data.HashMap.Strict qualified as DM
import Data.HashSet qualified as DS
import Data.List qualified as DL
import Game.Action (Action (..))
import Game.Letters as GL
import Game.Model (Model (..))
import Game.Model qualified as GM
import Game.Player (Player (..))
import Game.Tile (Status (..), Tile (..), emptyTiles, size, startingTiles)
import Game.Tile qualified as GT
import Game.View qualified as GV
import Miso (App, Effect, scripts, mount, styles, JS, CSS)
import Miso qualified as M
import Miso.State qualified as MS
import Miso.String qualified as MSS
import System.Random (StdGen)
import System.Random qualified as MR
import Prelude hiding (words)
import Miso.String(MisoString)
import Debug.Trace (traceShow)
import Game.Mode (Mode(Solo, NotPlaying))

default(MisoString)

maxReplaces ∷ Int
maxReplaces = 2

app ∷ [JS] -> [CSS] -> StdGen → App Model Action
app scripts styles generator = (M.component (GM.initModel generator) update GV.view){scripts = scripts, styles = styles}

update ∷ Action → Effect parent Model Action
update =
    \case
        NewGame → newGame
        SelectTile t → selectTile t
        ToggleTile t i → toggleTile t i
        ReplaceTiles → replaceTiles
        EndGame -> endGame

newGame ∷ Effect parent Model Action
newGame = do
    homeLetters ← produceTiles
    vowel ← initialVowel
    MS.modify $
        \m →
            m
                { board = GT.replaceAt 85 vowel emptyTiles
                , home = m.home{tiles = homeLetters}
                , mode = Solo
                }
  where
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

produceTiles ∷ (MonadState Model m) ⇒ m [Tile]
produceTiles = zipWith GT.bareTile [1 .. startingTiles] <$> randomLetters startingTiles

selectTile ∷ Tile → Effect parent Model Action
selectTile t = MS.modify $ \m → m{selected = Just t}

toggleTile ∷ Maybe Tile → Int → Effect parent Model Action
toggleTile t i = case t of
    Nothing → do
        model ← MS.get
        let tile = GT.tileAt model.board i
        CM.when (not (GT.isEmptyTile tile) && length model.home.tiles < startingTiles)
            . MS.modify
            $ \m →
                m
                    { board = checkBoard $ GT.replaceAt i 0 m.board
                    , home = m.home{tiles = GT.bareTile (length model.home.tiles + 1) tile.letter : m.home.tiles}
                    }
    Just tile → do
        model ← MS.get
        CM.when (canPlaceTile model.board) $ do
            let existing = (GT.tileAt model.board i).letter
                updatedBoard = checkBoard $ GT.replaceAt i tile.letter model.board
                updatedHome = filter (tile /=) model.home.tiles
            newLetters ← randomLetters $ startingTiles - length updatedHome
            MS.modify $ \m →
                m
                    { selected = Nothing
                    , board = updatedBoard
                    , home =
                        m.home
                            { score = makeScore updatedBoard
                            , tiles =
                                if existing > 0 then
                                    GT.bareTile tile.id existing : updatedHome
                                else
                                    if (GT.tileAt updatedBoard i).status == Valid then
                                        map (GT.bareTile tile.id) newLetters <> updatedHome
                                    else
                                        updatedHome
                            }
                    }
  where
    canPlaceTile = not . all GT.isEmptyTile . filter (\tl → tl.id == i + 1 || tl.id == i - 1 || tl.id == i - size || tl.id == i + size)

makeScore ∷ [Tile] → Int
makeScore = sum . map ms . filter (\t → t.status == Valid)
  where
    ms t
        | t.letter == letterA || t.letter == letterE || t.letter == letterI || t.letter == letterO || t.letter == letterU || t.letter == letterL || t.letter == letterN || t.letter == letterS || t.letter == letterT || t.letter == letterR = 1
        | t.letter == letterD || t.letter == letterG = 2
        | t.letter == letterB || t.letter == letterC || t.letter == letterM || t.letter == letterP = 3
        | t.letter == letterF || t.letter == letterH || t.letter == letterV || t.letter == letterW || t.letter == letterY = 4
        | t.letter == letterK = 5
        | t.letter == letterJ || t.letter == letterX = 8
        | t.letter == letterQ || t.letter == letterZ = 10

checkBoard ∷ [Tile] → [Tile]
checkBoard board = map check board
  where
    (valid, invalid) = DB.bimap DS.fromList DS.fromList $ checkWords board
    check t
        | DS.member t.id valid = t{status = Valid}
        | DS.member t.id invalid = t{status = Invalid}
        | otherwise = t

#ifdef WASM
foreign import javascript "return window.isValidWordLine($1);" isValidWord :: MisoString -> Bool
#else
isValidWord :: MisoString -> Bool
isValidWord _ = True
#endif

checkWords ∷ [Tile] → ([Int], [Int])
checkWords board = check words [] $ map (\[t] → t.id) straggles
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
        if GT.isEmptyTile f || f.id `mod` size == 0 then
            if null running then
                collectWords rom final running
            else
                collectWords rom (running : final) []
        else
            collectWords rom final (running <> [f])

    check ∷ [[Tile]] → [Int] → [Int] → ([Int], [Int])
    check [] valid invalid = (valid, invalid)
    check (w : ords) valid invalid = if isValidWord (MSS.concat $ map (GL.displayLetter . letter) w) then check ords (add w valid) invalid else check ords valid (add w invalid)

    add w list = map (\t → t.id) w <> list

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
        | otherwise = error "missing letter range!"

replaceTiles ∷ Effect parent Model Action
replaceTiles = do
    model ← MS.get
    CM.when (model.home.replaced < maxReplaces) $ do
        tiles ← produceTiles
        MS.modify $ \m → m{home = m.home{tiles = tiles, replaced = m.home.replaced + 1}}

endGame :: Effect parent Model Action
endGame = MS.modify $ \m -> m { mode = NotPlaying }
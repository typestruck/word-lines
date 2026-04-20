{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad qualified as CM
import Data.HashSet qualified as DS
import Game qualified as G
import Game.Letters (letterA, letterZ)
import Game.Tile (Tile, emptyTiles, id, size)
import Game.Tile qualified as GT
import Hedgehog (Gen, Property, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import Prelude hiding (words)

tile ∷ Gen Tile
tile = GT.bareTile <$> HG.int (HR.linear 1 $ size * size) <*> HG.int (HR.linear letterA letterZ)

prop_single_tiles_are_not_valid_words ∷ Property
prop_single_tiles_are_not_valid_words = H.withTests 1000 . H.property $ do
    t ← H.forAll tile
    G.checkWords (GT.replaceAt t.id t.letter emptyTiles) === (DS.fromList [], DS.fromList [t.id])

fillBoard ∷ [Tile] → [Tile]
fillBoard = foldl' (\board t → GT.replaceAt t.id t.letter board) emptyTiles

prop_words_are_two_or_more_contiguous_letters ∷ Property
prop_words_are_two_or_more_contiguous_letters = H.withTests 1000 . H.property $ do
    ts ← H.forAll . HG.list (HR.linear 2 (size - 2)) . HG.int $ HR.linear letterA letterZ
    -- cumbersome, but we need to guarantee that tiles do not cross into the next row or column as we test that separatedly
    ix ← H.forAll $ HG.choice [HG.int . HR.linear (s + 1) $ max (s + 1) $ s - length ts | s ← 1 : zipWith (\a b → a * b + 1) [1 ..] (replicate (size - 2) size)]
    iy ← H.forAll . HG.int . HR.linear 2 $ size * (size - length ts)
    let tiles = zipWith GT.bareTile (iterate (+ size) iy) ts <> zipWith GT.bareTile [ix ..] ts
    let (valid, invalid) = G.checkWords $ fillBoard tiles
    invalid === DS.fromList []
    valid === DS.fromList (map (\t → t.id) tiles)

prop_words_break_at_end_of_rows ∷ Property
prop_words_break_at_end_of_rows = H.withTests 1000 . H.property $ do
    ts ← H.forAll . HG.list (HR.singleton 5) . HG.int $ HR.linear letterA letterZ
    -- three tiles in one row, 2 in the other
    i ← H.forAll $ HG.choice [HG.int . HR.singleton $ s - 2 | s ← zipWith (*) [1 ..] $ replicate (size - 1) size]
    let words = G.collectWords (fillBoard $ zipWith GT.bareTile [i ..] ts) [] [] 1
    words === [zipWith GT.bareTile [i + 3 ..] $ drop 3 ts, zipWith GT.bareTile [i ..] $ take 3 ts]

tests ∷ IO Bool
tests = H.checkParallel $$H.discover

main ∷ IO ()
main = CM.void tests

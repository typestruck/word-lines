{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad qualified as CM
import Game.Letters (letterA, letterZ)
import Game.Tile (Tile, size, emptyTiles)
import Game.Tile qualified as GT
import Hedgehog
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import qualified Game as G

tile ∷ Gen Tile
tile = GT.bareTile <$> HG.int (HR.linear 1 size) <*> HG.int (HR.linear letterA letterZ)

prop_single_tiles_are_not_valid_words ∷ Property
prop_single_tiles_are_not_valid_words = withTests 1000 . property $ do
    t ← forAll tile
    G.checkWords (GT.replaceAt t.id t.letter emptyTiles) === ([], [t.id])

tests ∷ IO Bool
tests =
    checkParallel $$(discover)

main ∷ IO ()
main = CM.void tests

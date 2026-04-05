{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad qualified as CM
import Game qualified as G
import Game.Letters (letterA, letterZ)
import Game.Tile (Tile, emptyTiles, size)
import Game.Tile qualified as GT
import Hedgehog (Gen, Property, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR

tile ∷ Gen Tile
tile = GT.bareTile <$> HG.int (HR.linear 1 $ size * size) <*> HG.int (HR.linear letterA letterZ)

prop_single_tiles_are_not_valid_words ∷ Property
prop_single_tiles_are_not_valid_words = H.withTests 1000 . H.property $ do
    t ← H.forAll tile
    G.checkWords (GT.replaceAt t.id t.letter emptyTiles) === ([], [t.id])

prop_words_are_two_or_more_contiguous_letter ∷ Property
prop_words_are_two_or_more_contiguous_letter = H.withTests 1000 . H.property $ do
    ts ← H.forAll $ HG.list (HR.linear 2 size) tile
    let (valid, invalid) = G.checkWords . snd $ foldl' (\(i, b) t -> (i + 1 , GT.replaceAt i t.letter b)) (1, emptyTiles) ts
    invalid === []
    length valid === length ts

tests ∷ IO Bool
tests = H.checkParallel $$H.discover

main ∷ IO ()
main = CM.void tests

module Game.Player where

import Game.Tile (Tile)

data Player = Player {tiles ∷ [Tile], score ∷ Int, replaced ∷ Int} deriving (Show, Eq)

barePlayer ∷ Player
barePlayer = Player{tiles = [], replaced = 0, score = 0}

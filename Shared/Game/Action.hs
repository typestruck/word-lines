module Game.Action where

import Game.Tile (Tile)

data Action
    = CheckAssets
    | NewGame
    | SelectTile Tile
    | ToggleTile (Maybe Tile) Int
    | ReplaceTiles
    | EndGame
    deriving (Show, Eq)

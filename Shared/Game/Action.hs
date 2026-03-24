module Game.Action where

import Game.Tile (Tile)

data Action
    =
      NewGame
    | SelectTile Tile
    | ToggleTile (Maybe Tile) Int
    | ReplaceTiles
    deriving (Show, Eq)

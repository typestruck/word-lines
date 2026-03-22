module Game.Tile where

data Status = Valid | Invalid deriving (Show, Eq)

data Tile = Tile {id ∷ Int, letter ∷ Int, status ∷ Status} deriving (Show, Eq)

size ∷ Int
size = 13

startingTiles ∷ Int
startingTiles = 8

emptyTiles ∷ [Tile]
emptyTiles = zipWith bareTile [1 .. size * size] $ replicate (size * size) 0

bareTile ∷ Int → Int → Tile
bareTile i l = Tile{id = i, letter = l, status = Invalid}

tileAt ∷ [Tile] → Int → Tile
tileAt board i = board !! (i - 1)

isEmptyTile ∷ Tile → Bool
isEmptyTile t = t.letter == 0

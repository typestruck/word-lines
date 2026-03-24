module Game.Model where

import Game.Player (Player (..), barePlayer)
import Game.Tile (Tile (..), emptyTiles)
import System.Random (StdGen)

data Model = Model
    { board ∷ [Tile]
    , home ∷ Player
    , away ∷ Player
    , selected ∷ Maybe Tile
    , generator ∷ StdGen
    }

instance Eq Model where
    m == n = m.board == n.board && m.home == n.home && m.away == n.away && m.selected == n.selected

initModel ∷ StdGen → Model
initModel generator = Model{board = emptyTiles, home = barePlayer, away = barePlayer, generator = generator, selected = Nothing}

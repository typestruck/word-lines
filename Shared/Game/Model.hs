module Game.Model where

import Data.HashSet (HashSet)
import Data.Text (Text)
import Game.Player (Player (..), barePlayer)
import Game.Tile (Tile (..), emptyTiles)
import System.Random (StdGen)

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

initModel ∷ StdGen → HashSet Text → Model
initModel generator dictionary = Model{board = emptyTiles, home = barePlayer, away = barePlayer, generator = generator, dictionary = dictionary, selected = Nothing}

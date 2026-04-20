module Game.Model where

import Game.Player (Player (..), barePlayer)
import Game.Tile (Tile (..), emptyTiles)
import System.Random (StdGen)
import Game.Mode(Mode(..))

data Model = Model
    { board ∷ [Tile]
    , home ∷ Player
    , mode :: Mode
    , selected ∷ Maybe Tile
    , generator ∷ StdGen
    , assetsLoaded :: Bool
    }

instance Eq Model where
    m == n = m.board == n.board && m.home == n.home && m.selected == n.selected && m.mode == n.mode && m.assetsLoaded == n.assetsLoaded

initModel ∷ StdGen → Model
initModel generator = Model{ mode = NotPlaying, assetsLoaded = False, board = emptyTiles, home = barePlayer, generator = generator, selected = Nothing}

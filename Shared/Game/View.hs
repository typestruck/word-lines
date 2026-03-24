{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.View where

import Data.List qualified as DL
import Game.Action (Action (..))
import Game.Letters qualified as GL
import Game.Mode (Mode (..))
import Game.Model (Model (..))
import Game.Player (Player (..))
import Game.Tile (Status (..))
import Game.Tile qualified as GT
import Miso (MisoString, View)
import Miso qualified as M
import Miso.Html.Element qualified as HE
import Miso.Html.Event qualified as HP
import Miso.Html.Property qualified as HP
import Miso.String qualified as MSS

default (MisoString)

view ∷ Model → View Model Action
view model =
    HE.main_ [] $ case model.mode of
        NotPlaying → notPlaying model
        Solo → solo model

notPlaying ∷ Model → [View Model Action]
notPlaying _ =
    [ HE.div_
        []
        [ HE.h1_ [] [M.text "word-lines"]
        , HE.div_ [] [HE.button_ [HP.onClick NewGame] [M.text "Play solo"]]
        ]
    ]

solo ∷ Model → [View Model Action]
solo model =
    [ HE.div_
        [HP.className "left-side"]
        [ HE.div_ [HP.className "board"] $ map (\t → makeTile (ToggleTile model.selected t.id) t) model.board
        , HE.div_ [HP.className "home-tiles"] $ map (\t → makeTile (SelectTile t) t) $ DL.sortBy alpha model.home.tiles
        ]
    , HE.div_
        [HP.className "right-side"]
        [ HE.div_
            [HP.className "submit-button"]
            [ HE.label_ [] [M.text $ "Score: " <> MSS.pack (show model.home.score)]
            ]
        , HE.div_
            [HP.className "submit-button"]
            [ HE.button_ [HP.className "submit", HP.onClick EndGame] [M.text "End game"]
            ]
        , HE.button_ [HP.className "submit", HP.onClick ReplaceTiles] [M.text "Replace"]
        ]
    ]
  where
    alpha t u = compare t.letter u.letter

    makeTile action t = HE.div_ [HP.classList_ [("tile", True), ("valid", t.status == Valid)], HP.onClick action] [M.text $ if GT.isEmptyTile t then "" else GL.displayLetter t.letter]

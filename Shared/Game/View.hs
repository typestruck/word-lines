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
    HE.main_ [HP.className "dark:bg-gray-800 dark:text-white min-h-full"] $ case model.mode of
        NotPlaying → notPlaying model
        Solo → solo model

notPlaying ∷ Model → [View Model Action]
notPlaying _ =
    [ HE.div_
        [HP.className "flex flex-col items-center pt-30"]
        [ HE.h1_ [HP.className "text-4xl font-bold"] [M.text "word lines"]
        , HE.button_ [HP.className "p-10 pl-15 pr-15 text-xl bg-green-600 font-bold mt-15 rounded-sm", HP.onClick NewGame] [M.text "Play solo"]
        ]
    ]

solo ∷ Model → [View Model Action]
solo model =
    [ HE.div_
        [HP.className "flex"]
        [ HE.div_
            [HP.className "flex flex-col m-20"]
            [ HE.div_ [HP.className "border-1 text- dark:border-neutral-300 dark:bg-white grid grid-cols-[repeat(13,_60px)] grid-rows-[repeat(13,_60px)] gap-1"] $ map (\t → makeTile (ToggleTile model.selected t.id) t) model.board
            , HE.div_ [HP.className "border-1 dark:border-neutral-300 mt-20 dark:bg-white grid grid-cols-[repeat(13,_60px)] grid-rows-[repeat(1,_60px)] gap-1"] $ map (\t → makeTile (SelectTile t) t) $ DL.sortBy alpha model.home.tiles
            ]
        , HE.div_
            [HP.className "flex flex-col"]
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
    ]
  where
    alpha t u = compare t.letter u.letter

    makeTile action t = HE.div_ [HP.classList_ [("text-2xl dark:bg-gray-800 text-red-50", True), ("text-inherit", t.status == Valid)], HP.onClick action] [M.text $ if GT.isEmptyTile t then "" else GL.displayLetter t.letter]

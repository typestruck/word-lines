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
    HE.main_ [HP.className "dark:bg-gray-800 dark:text-white min-h-full flex flex-col"] $ case model.mode of
        NotPlaying → notPlaying model
        Solo → solo model

notPlaying ∷ Model → [View Model Action]
notPlaying _ =
    [ HE.div_
        [HP.className "flex flex-col self-center pt-30 w-fit"]
        [ HE.h1_ [HP.className "text-4xl font-bold text-center"] [M.text "word lines"]
        , HE.button_ [HP.className "p-10 pl-15 pr-15 text-xl bg-green-600 font-bold mt-15 rounded-sm", HP.onClick NewGame] [M.text "Play solo"]
        , HE.button_ [HP.className "p-10 pl-15 pr-15 text-xl bg-green-600 font-bold mt-15 rounded-sm"] [M.text "Play vs (coming soon!)"]
        , HE.button_ [HP.className "p-10 pl-15 pr-15 text-xl bg-green-600 font-bold mt-15 rounded-sm"] [M.text "Play vs computer (coming soon!)"]
        ]
    ]

solo ∷ Model → [View Model Action]
solo model =
    [ HE.div_
        [HP.className "flex flex-col md:flex-row"]
        [ HE.div_
            [HP.className "flex flex-col m-10 mt-20 mb-20 md:m-20"]
            [ HE.div_ [HP.className "border-1 dark:border-neutral-500 dark:bg-neutral-500 grid grid-flow-dense auto-rows-[minmax(0,auto)] grid-cols-[repeat(11,minmax(0,1fr))] grid-rows-[repeat(11,33px)] md:grid-cols-[repeat(11,60px)] md:grid-rows-[repeat(11,60px)] gap-1"] $ map (\t → makeTile (ToggleTile model.selected t.id) t) model.board
            , HE.div_
                [HP.className "flex items-center"]
                [ HE.div_ [HP.className "border-1 dark:border-neutral-500 mt-20 grid-flow-dense auto-rows-[minmax(0,auto)] dark:bg-neutral-500 grid grid-cols-[repeat(8,33px)] grid-rows-[repeat(1,33px)] md:grid-rows-[repeat(1,60px)] md:grid-cols-[repeat(8,60px)] gap-1 w-fit"] $ map (\t → makeTile (SelectTile t) t) $ DL.sortBy alpha model.home.tiles
                , HE.div_ [HP.className "ml-auto"] [HE.button_ [HP.className "p-5 pl-10 pr-10 bg-green-600 mt-15 rounded-sm", HP.onClick ReplaceTiles] [M.text "Random"]]
                ]
            ]
        , HE.div_
            [HP.className "flex flex-col justify-end m-10 mb:m-10 mb-100"]
            [ HE.label_ [] [M.text $ "Score: " <> MSS.pack (show model.home.score)]
            , HE.button_ [HP.className "p-5 pl-10 pr-10 bg-green-600 md:mt-15 rounded-sm w-fit", HP.onClick EndGame] [M.text "End game"]
            ]
        ]
    ]
  where
    alpha t u = compare t.letter u.letter

    makeTile action t = HE.div_ [HP.classList_ [("md:text-2xl font-bold dark:bg-gray-800 flex justify-center items-center", True), ("text-red-500", t.status == Invalid && action /= SelectTile t)], HP.onClick action] [M.text $ if GT.isEmptyTile t then "" else GL.displayLetter t.letter]

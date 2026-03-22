{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class qualified as CMIC
import Data.HashSet qualified as DS
import Game.Model (Model)
import Game.Model qualified as GM
import Game.View qualified as GV
import Miso.Html.Render (ToHtml)
import Miso.Html.Render qualified as MHR
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp qualified as W
import Servant
import System.Random qualified as SR

data Home = Home Model

data Html

instance Accept Html where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance (ToHtml a) ⇒ MimeRender Html a where
    mimeRender _ = MHR.toHtml

instance ToHtml Home where
    toHtml (Home model) = MHR.toHtml $ GV.view model

type WordLinesApi = Get '[Html] Home

server ∷ Server WordLinesApi
server = do
    generator ← CMIC.liftIO SR.newStdGen
    pure . Home $ GM.initModel generator DS.empty

wordLinesApi ∷ Proxy WordLinesApi
wordLinesApi = Proxy

app1 ∷ Application
app1 = serve wordLinesApi server

main ∷ IO ()
main = do
    putStrLn "Server running at http://localhost:8081"
    W.run 8081 app1

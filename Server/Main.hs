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
import Network.Wai.Handler.Warp qualified as W
import Servant
import System.Random qualified as SR

data Home = Home Model

data Html

instance Accept Html where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance (ToHtml a) ⇒ MimeRender Html a where
    mimeRender _ = fullPage .  MHR.toHtml
        where fullPage bs = "<!doctype html><html lang=en><head><meta name='viewport' content='width=device-width, initial-scale=1'><meta charset=utf-8><title>word lines</title></head><body>" <> bs <> "</body><script src='static/index.js' type='module'></script></html>"

instance ToHtml Home where
    toHtml (Home model) = MHR.toHtml $ GV.view model

type WordLinesApi = Get '[Html] Home :<|> "static" :> Raw

handlers :: Server WordLinesApi
handlers = homeHandler :<|> staticHandler
    where
    homeHandler = do
        generator ← CMIC.liftIO SR.newStdGen
        pure . Home $ GM.initModel generator DS.empty

    staticHandler = serveDirectoryWebApp "public"

app ∷ Application
app = serve (Proxy :: Proxy WordLinesApi) handlers

main ∷ IO ()
main = do
    putStrLn "Server running at http://localhost:8081"
    W.run 8081 app

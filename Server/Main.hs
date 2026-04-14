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
import Game.Model (Model)
import Game.Model qualified as GM
import Game.View qualified as GV
import Miso.Html.Render (ToHtml)
import Miso.Html.Render qualified as MHR
import Network.HTTP.Media ((//), (/:))
import Network.Wai qualified as NW
import Network.Wai.Handler.Warp qualified as NWHW
import Servant (
    Accept (contentType),
    Application,
    Get,
    MimeRender (..),
    Proxy (..),
    Raw,
    Server,
    type (:<|>) (..),
    type (:>),
 )
import Servant qualified as S
import System.Random qualified as SR
import Debug.Trace (trace)

newtype Home = Home Model

data Html

instance Accept Html where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance (ToHtml a) ⇒ MimeRender Html a where
    mimeRender _ = fullPage . MHR.toHtml
      where
        fullPage bs = "<!doctype html><html lang=en><head><meta name='viewport' content='width=device-width, initial-scale=1'><meta charset=utf-8><title>word lines</title><link rel='stylesheet' href='static/styles.css?q=s33'/><script src='static/dictionary.js' async></script></head><body>" <> bs <> "</body><script src='static/index.js' type='module'></script></html>"

instance ToHtml Home where
    toHtml (Home model) = MHR.toHtml $ GV.view model

type WordLinesApi = Get '[Html] Home :<|> "static" :> Raw

handlers ∷ Server WordLinesApi
handlers = homeHandler :<|> staticHandler
  where
    homeHandler = do
        generator ← CMIC.liftIO SR.newStdGen
        pure . Home $ GM.initModel generator

    staticHandler = addCors <$> S.serveDirectoryWebApp "public/static"
    addCors app req sendResponse = app req $ \res → sendResponse $ NW.mapResponseHeaders (("Access-Control-Allow-Origin", "localhost:8080") :) res

application ∷ Application
application = S.serve (Proxy ∷ Proxy WordLinesApi) handlers

main ∷ IO ()
main = do
    putStrLn "Server running at http://localhost:8081!!!!"
    NWHW.run 8081 application

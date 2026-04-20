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
        fullPage bs = "<!doctype html><html lang=en><head><meta name='viewport' content='width=device-width, initial-scale=1'><meta charset=utf-8><title>word lines</title><style>.hidden { display: none} .loading-box { display:flex;align-items:center;position:absolute;bottom:10px;right:10px;border-radius:2px;color:white;background:purple;font-weight:bold;padding:10px } .loader { margin-right:5px;border: 8px solid #f3f3f3; border-top: 8px solid oklch(62.7% 0.194 149.214); border-radius: 50%;width: 35px;height: 35px;animation: spin 2s linear infinite;} @keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); }}</style><link rel='stylesheet' href='static/styles.css?b=a3'/><script src='static/dictionary.js' async></script></head><body>" <> bs <> "</body><script src='static/index.js?d=v' type='module'></script></html>"

instance ToHtml Home where
    toHtml (Home model) = MHR.toHtml $ GV.view model

type WordLinesApi = Get '[Html] Home :<|> "static" :> Raw

handlers ∷ Server WordLinesApi
handlers = homeHandler :<|> staticHandler
  where
    homeHandler = do
        generator ← CMIC.liftIO SR.newStdGen
        pure . Home $ GM.initModel generator
    -- only used for development
    staticHandler = addCors <$> S.serveDirectoryWebApp "public/static"
    addCors app req sendResponse = app req $ \res → sendResponse $ NW.mapResponseHeaders (("Access-Control-Allow-Origin", "localhost:8080") :) res

application ∷ Application
application = S.serve (Proxy ∷ Proxy WordLinesApi) handlers

main ∷ IO ()
main = do
    putStrLn "Server running at http://localhost:8081"
    NWHW.run 8081 application

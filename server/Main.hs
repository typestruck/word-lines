{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import qualified Lucid as L
import Servant.HTML.Lucid ( HTML )
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import qualified Network.Wai.Handler.Warp as W
import Servant
import System.Directory
import Servant.Types.SourceT (source)
import Lucid (ToHtml)

data Home = Home

instance ToHtml Home where
    toHtml _ = L.div_ $ do
        L.h1_ "word lines"
        L.div_ $ L.button_ "Play solo"

    toHtmlRaw = L.toHtml

type WordLinesApi =  Get '[HTML] Home

server :: Server WordLinesApi
server = pure Home

wordLinesApi :: Proxy WordLinesApi
wordLinesApi = Proxy

app1 :: Application
app1 = serve wordLinesApi server

main :: IO ()
main = do
    putStrLn "Server running at http://localhost:8081"
    W.run 8081 app1
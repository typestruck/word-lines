{-# LANGUAGE CPP #-}

module Main where

import Miso qualified as M
import Miso.String qualified as MS
import qualified Game as G
import System.Random qualified as MR
import Miso (defaultEvents, JS (Src), MisoString, CSS (Href))

#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif

main ∷ IO ()
#ifdef INTERACTIVE
main = do
    generator ← MR.newStdGen
    M.reload defaultEvents $ G.app [Src (MS.pack "http://localhost:8081/static/dictionary.js") False] [Href (MS.pack "http://localhost:8081/static/styles.css") False] generator
#else
main = do
    generator ← MR.newStdGen
    M.prerender defaultEvents $ G.app [] [] generator
#endif



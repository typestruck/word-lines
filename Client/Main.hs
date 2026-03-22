{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Dictionary (englishDictionary)
import Miso qualified as M
import qualified Game as G
import System.Random qualified as MR
import Miso (defaultEvents)

#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif

main ∷ IO ()
#ifdef INTERACTIVE
main = do
    generator ← MR.newStdGen
    M.reload defaultEvents $ G.app englishDictionary generator
#else
main = do
    generator ← MR.newStdGen
    M.prerender defaultEvents $ G.app englishDictionary generator
#endif



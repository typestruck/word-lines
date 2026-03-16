{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Dictionary where

import Data.HashSet (HashSet)
import Data.HashSet qualified as DS
import Data.Text (Text)

default (Text)

englishDictionary ∷ HashSet Text
englishDictionary = DS.fromList ["MOO", "MOOD", "MAD", "PI", "HI", "POST"]

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Letters where

import Miso (MisoString)

default (MisoString)

type Letter = Int

letterE :: Letter
letterE = 69

letterI :: Letter
letterI = 73

letterA :: Letter
letterA = 65

letterO :: Letter
letterO = 79

letterR :: Letter
letterR = 82

letterN :: Letter
letterN = 78

letterT :: Letter
letterT = 84

letterS :: Letter
letterS = 83

letterL :: Letter
letterL = 76

letterC :: Letter
letterC = 67

letterU :: Letter
letterU = 85

letterP :: Letter
letterP = 80

letterM :: Letter
letterM = 77

letterD :: Letter
letterD = 68

letterH :: Letter
letterH = 72

letterY :: Letter
letterY = 89

letterG :: Letter
letterG = 71

letterB :: Letter
letterB = 66

letterF :: Letter
letterF = 70

letterV :: Letter
letterV = 86

letterK :: Letter
letterK = 75

letterW :: Letter
letterW = 87

letterZ :: Letter
letterZ = 90

letterX :: Letter
letterX = 88

letterQ :: Letter
letterQ = 81

letterJ :: Letter
letterJ = 74

displayLetter :: Letter → MisoString
displayLetter = \case
    69 → "E"
    73 → "I"
    65 → "A"
    79 → "O"
    82 → "R"
    78 → "N"
    84 → "T"
    83 → "S"
    76 → "L"
    67 → "C"
    85 → "U"
    80 → "P"
    77 → "M"
    68 → "D"
    72 → "H"
    89 → "Y"
    71 → "G"
    66 → "B"
    70 → "F"
    86 → "V"
    75 → "K"
    87 → "W"
    90 → "Z"
    88 → "X"
    81 → "Q"
    74 → "J"
    b → error $ show b

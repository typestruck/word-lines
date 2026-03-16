{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Letters where

import Miso (MisoString)

default (MisoString)

letterE :: Int
letterE = 69

letterI :: Int
letterI = 73

letterA :: Int
letterA = 65

letterO :: Int
letterO = 79

letterR :: Int
letterR = 82

letterN :: Int
letterN = 78

letterT :: Int
letterT = 84

letterS :: Int
letterS = 83

letterL :: Int
letterL = 76

letterC :: Int
letterC = 67

letterU :: Int
letterU = 85

letterP :: Int
letterP = 80

letterM :: Int
letterM = 77

letterD :: Int
letterD = 68

letterH :: Int
letterH = 72

letterY :: Int
letterY = 89

letterG :: Int
letterG = 71

letterB :: Int
letterB = 66

letterF :: Int
letterF = 70

letterV :: Int
letterV = 86

letterK :: Int
letterK = 75

letterW :: Int
letterW = 87

letterZ :: Int
letterZ = 90

letterX :: Int
letterX = 88

letterQ :: Int
letterQ = 81

letterJ :: Int
letterJ = 74

displayLetter :: Int -> MisoString
displayLetter = \case
    69 -> "E"
    73 -> "I"
    65 -> "A"
    79 -> "O"
    82 -> "R"
    78 -> "N"
    84 -> "T"
    83 -> "S"
    76 -> "L"
    67 -> "C"
    85 -> "U"
    80 -> "P"
    77 -> "M"
    68 -> "D"
    72 -> "H"
    89 -> "Y"
    71 -> "G"
    66 -> "B"
    70 -> "F"
    86 -> "V"
    75 -> "K"
    87 -> "W"
    90 -> "Z"
    88 -> "X"
    81 -> "Q"
    74 -> "J"
    b -> error $ show b

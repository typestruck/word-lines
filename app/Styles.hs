{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Styles where
import Miso (CSS (Style), MisoString)

default (MisoString)

styleSheet :: CSS
styleSheet = Style """

html {
    height: 100%;
    box-sizing: border-box;
}

body {
    background: #1D2B35;
    color: #FBFEFB;
    width: 99vw;
    font-size:17px;
    height: 99vh;
}

main {
    margin-left:20px;
    display: flex;
}

.board {
    margin-top:40px;
    display: grid;
    grid-template-columns: repeat(13, 60px);
    grid-template-rows: repeat(13, 60px);
    gap:3px;
}

.home-tiles {
    margin-top:40px;
    display: grid;
    gap:3px;
    grid-template-columns: repeat(13, 60px);
    grid-auto-flow: dense;
    grid-template-rows: repeat(1, 60px);
}

.tile {
    justify-content: center;
    align-items: center;
    font-weight:bold;
    display:flex;
    background: #393E41;
    font-size:1.6em
}

.board .tile {
    color: red;
}

.right-side {
    display: flex;
    justify-content: end;
    flex-direction: column;
    margin-left:20px
}

.submit {
    font-size:1.6em
}

.submit-button {
    margin-bottom:20px
}

.board .tile.valid {
    color: inherit
}

"""
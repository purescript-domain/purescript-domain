module Domain.Homepage.Theme where

import Color (Color, rgb)
import Data.Tuple.Nested (type (/\), (/\))
import Tecton (monospace, sansSerif)
import Type.Proxy (Proxy)

lightGray = rgb 132 136 144 :: Color

darkGray = rgb 27 31 40 :: Color

darkerGray = rgb 13 13 18 :: Color

lightGold = rgb 246 204 128 :: Color

gold = rgb 188 138 51 :: Color

montserrat :: String /\ Proxy "sans-serif"
montserrat = "Montserrat" /\ sansSerif

inconsolata :: String /\ Proxy "monospace"
inconsolata = "Inconsolata" /\ monospace

roboto :: String /\ Proxy "sans-serif"
roboto = "Roboto" /\ sansSerif
module Domain.Homepage.Link (block, css, css') where

import Prelude

import Domain.Homepage.Theme as Theme
import Tecton
  ( CSS
  , backgroundColor
  , boxShadow
  , color
  , focus
  , hover
  , nil
  , outlineWidth
  , px
  , textDecorationLine
  , underline
  , universal
  , (&:)
  , (:=)
  , (?)
  , (~)
  )
import Tecton.Halogen ((&.))
import Tecton.Internal (Extensible, Selector)
import Tecton.Rule as Rule
import Web.HTML.Common (ClassName(..))

block = ClassName "link" :: ClassName

css' :: Selector Extensible -> CSS
css' sel = do
  sel ? Rule.do
    color := Theme.gold
    textDecorationLine := underline
    outlineWidth := nil
  sel &: hover ? Rule.do
    color := Theme.lightGold
  sel &: focus ? Rule.do
    backgroundColor := Theme.darkerGray
    boxShadow := Theme.darkerGray ~ nil ~ nil ~ nil ~ px 4

css :: CSS
css = css' $ universal &. block
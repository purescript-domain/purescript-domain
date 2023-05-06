module Domain.Homepage.Logo
  ( UseLogo
  , css
  , useLogo
  ) where

import Prelude

import Color (white)
import Data.Tuple.Nested ((/\))
import Domain.Homepage.Route as Route
import Domain.Homepage.Theme as Theme
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (class HookNewtype, Hook, HookType, Pure)
import Halogen.Hooks as Hooks
import Tecton
  ( CSS
  , backgroundColor
  , boxShadow
  , color
  , display
  , em
  , focus
  , fontFamily
  , fontSize
  , fontWeight
  , inlineBlock
  , lineHeight
  , margin
  , nil
  , none
  , normal
  , nowrap
  , outlineStyle
  , position
  , px
  , relative
  , scale
  , textDecorationLine
  , textTransform
  , transform
  , translateY
  , universal
  , uppercase
  , whiteSpace
  , (&:)
  , (:=)
  , (?)
  , (~)
  )
import Tecton.Halogen ((&.))
import Tecton.Rule as Rule

block = ClassName "logo" :: ClassName

textElement = ClassName "logo__text" :: ClassName

dotElement = ClassName "logo__dot" :: ClassName

css :: CSS
css = do
  universal &. block ? Rule.do
    display := inlineBlock
    color := white
    textDecorationLine := none
    whiteSpace := nowrap
    outlineStyle := none
  universal &. block &: focus ? Rule.do
    backgroundColor := Theme.darkerGray
    boxShadow := Theme.darkerGray ~ nil ~ nil ~ nil ~ px 4
  universal &. textElement ? Rule.do
    position := relative
    margin := nil
    fontFamily := Theme.montserrat
    fontSize := em 2.5
    fontWeight := normal
    textTransform := uppercase
    lineHeight := 1
  universal &. dotElement ? Rule.do
    display := inlineBlock
    transform := translateY (em (-0.6)) /\ scale 2.0 2.0
    margin := nil ~ em 0.125
    color := Theme.gold

foreign import data UseLogo :: HookType

instance HookNewtype UseLogo Pure

useLogo :: forall m w i. { link :: Boolean } -> Hook m UseLogo (HH.HTML w i)
useLogo { link } =
  let
    block' =
      if link then HH.a
        [ HP.class_ block, HP.href $ "#" <> Route.print Route.Home ]
      else HH.div [ HP.class_ block ]
  in
    Hooks.wrap
      $ Hooks.pure
      $
        block'
          [ HH.h1
              [ HP.class_ textElement ]
              [ HH.text "purescri"
              , HH.span [ HP.class_ dotElement ] [ HH.text "." ]
              , HH.text "pt"
              ]
          ]
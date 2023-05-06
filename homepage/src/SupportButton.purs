module Domain.Homepage.SupportButton
  ( UseSupportButton
  , css
  , useSupportButton
  ) where

import Prelude

import Color (black, white)
import Domain.Homepage.Theme as Theme
import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (class HookNewtype, Hook, HookType, Pure)
import Halogen.Hooks as Hooks
import Tecton
  ( CSS
  , alignItems
  , backgroundColor
  , borderWidth
  , boxShadow
  , center
  , color
  , display
  , em
  , focus
  , fontFamily
  , fontSize
  , height
  , inherit
  , inlineFlex
  , justifyContent
  , lineHeight
  , margin
  , nil
  , none
  , outlineWidth
  , padding
  , px
  , textDecorationLine
  , universal
  , width
  , (&:)
  , (:=)
  , (?)
  , (~)
  )
import Tecton.Halogen ((&.))
import Tecton.Rule as Rule

block = ClassName "support-button" :: ClassName
iconElement = ClassName "support-button__icon" :: ClassName
iconWrapElement = ClassName "support-button__icon-wrap" :: ClassName
labelElement = ClassName "support-button__label" :: ClassName

css :: CSS
css = do
  universal &. block ? Rule.do
    borderWidth := nil
    outlineWidth := nil
    backgroundColor := black
    color := white
    margin := nil
    padding := nil
    fontFamily := Theme.roboto
    fontSize := inherit
    lineHeight := 1
    textDecorationLine := none
    display := inlineFlex
    alignItems := center
  universal &. block &: focus ? Rule.do
    boxShadow := Theme.gold ~ nil ~ nil ~ nil ~ px 1
  universal &. iconWrapElement ? Rule.do
    width := em 1.5
    height := em 1.5
    backgroundColor := Theme.gold
    display := inlineFlex
    alignItems := center
    justifyContent := center
  universal &. iconElement ? Rule.do
    width := em 1
    height := em 1
  universal &. labelElement ? Rule.do
    padding := nil ~ em 0.5

foreign import data UseSupportButton :: HookType

instance HookNewtype UseSupportButton Pure

useSupportButton
  :: forall m w i
   . { url :: String, imageURL :: String, label :: String }
  -> Hook m UseSupportButton (HH.HTML w i)
useSupportButton { url, imageURL, label } =
  Hooks.wrap
    $ Hooks.pure
    $
      HH.a
        [ HP.href url, HP.target "_blank", HP.class_ block ]
        [ HH.div
            [ HP.class_ iconWrapElement ]
            [ HH.img [ HP.class_ iconElement, HP.src imageURL, HP.alt "" ]
            ]
        , HH.div
            [ HP.class_ labelElement ]
            [ HH.text label ]
        ]
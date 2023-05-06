module Domain.Homepage.Header
  ( UseHeader
  , css
  , useHeader
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Domain.Homepage.Link as Link
import Domain.Homepage.Logo (UseLogo, useLogo)
import Domain.Homepage.ResponsiveRow (UseResponsiveRow2, useResponsiveRow2)
import Domain.Homepage.Route (Route)
import Domain.Homepage.Route as Route
import Domain.Homepage.SupportButtons (UseSupportButtons, useSupportButtons)
import Effect.Class (class MonadEffect)
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookType, Pure)
import Halogen.Hooks as Hooks
import Tecton
  ( CSS
  , display
  , em
  , grid
  , margin
  , minHeight
  , px
  , universal
  , (:=)
  , (?)
  )
import Tecton.Halogen ((&.))
import Tecton.Rule as Rule

block = ClassName "header" :: ClassName

supportButtonsWrapElement =
  ClassName "header__support-buttons-wrap" :: ClassName

css :: CSS
css = do
  universal &. block ? Rule.do
    minHeight := em 2.5
    display := grid
  universal &. supportButtonsWrapElement ? Rule.do
    margin := px 1

foreign import data UseHeader :: HookType

instance
  HookNewtype UseHeader
    (UseLogo <> UseSupportButtons <> UseResponsiveRow2 <> Pure)

useHeader
  :: forall m w i
   . MonadEffect m
  => Maybe Route
  -> Hook m UseHeader (HH.HTML w i)
useHeader route =
  Hooks.wrap $
    Hooks.do
      logo <- useLogo { link: true }
      supportButtons <- useSupportButtons
      row <-
        useResponsiveRow2
          ( case route of
              Just Route.Home ->
                HH.a
                  [ HP.href $ "#" <> Route.print Route.HowItWorks
                  , HP.class_ Link.block
                  ]
                  [ HH.text "How it works" ]
              _ ->
                logo
          )
          (HH.div [ HP.class_ supportButtonsWrapElement ] [ supportButtons ])
      Hooks.pure $
        HH.div
          [ HP.class_ block ]
          [ row ]
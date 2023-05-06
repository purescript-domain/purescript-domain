module Domain.Homepage.Home
  ( component
  , css
  ) where

import Prelude

import Domain.Homepage.SearchBox (useSearchBox)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Tecton
  ( CSS
  , alignItems
  , center
  , display
  , em
  , flex
  , justifyContent
  , maxHeight
  , universal
  , (:=)
  , (?)
  )
import Tecton.Halogen ((&.))
import Tecton.Rule as Rule

block = ClassName "home" :: ClassName

css :: CSS
css = do
  universal &. block ? Rule.do
    maxHeight := em 40
    display := flex
    alignItems := center
    justifyContent := center

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  Hooks.component \_ _ -> Hooks.do
    searchBox <- useSearchBox
    Hooks.pure $
      HH.div [ HP.class_ block ] [ searchBox ]
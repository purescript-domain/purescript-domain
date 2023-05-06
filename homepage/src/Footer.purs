module Domain.Homepage.Footer
  ( UseFooter
  , css
  , useFooter
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Domain.Homepage.Link as Link
import Domain.Homepage.ResponsiveRow (UseResponsiveRow2, useResponsiveRow2)
import Domain.Homepage.Route (Route(..))
import Domain.Homepage.Route as Route
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
  , fontSize
  , gap
  , inlineFlex
  , universal
  , (:=)
  , (?)
  )
import Tecton.Halogen ((&.))
import Tecton.Rule as Rule

block = ClassName "footer" :: ClassName

linksElement = ClassName "footer__links" :: ClassName

css :: CSS
css = do
  universal &. block ? Rule.do
    fontSize := em 0.75
  universal &. linksElement ? Rule.do
    display := inlineFlex
    gap := em 0.5

foreign import data UseFooter :: HookType

instance HookNewtype UseFooter (UseResponsiveRow2 <> Pure)

useFooter
  :: forall m w i
   . MonadEffect m
  => Maybe Route
  -> Hook m UseFooter (HH.HTML w i)
useFooter route =
  Hooks.wrap $
    Hooks.do
      row <-
        useResponsiveRow2
          (HH.text "Copyright © 2022-2023 PureScript Domain")
          ( HH.div
              [ HP.class_ linksElement ]
              [ case route of
                  Just Terms ->
                    HH.text "Terms and Conditions"
                  _ ->
                    HH.a
                      [ HP.href $ "#" <> Route.print Terms
                      , HP.class_ Link.block
                      ]
                      [ HH.text "Terms and Conditions" ]
              , HH.a
                  [ HP.href "https://github.com/purescript-domains"
                  , HP.target "_blank"
                  , HP.class_ Link.block
                  ]
                  [ HH.text "GitHub" ]
              , HH.a
                  [ HP.href "https://twitter.com/pursdomains"
                  , HP.target "_blank"
                  , HP.class_ Link.block
                  ]
                  [ HH.text "Twitter" ]
              ]
          )
      Hooks.pure $
        HH.footer
          [ HP.class_ block ]
          [ row ]
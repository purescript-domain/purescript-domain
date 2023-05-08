module Domain.Homepage.App
  ( Query(..)
  , component
  , css
  ) where

import Prelude

import Color (white)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Domain.Homepage.Footer (useFooter)
import Domain.Homepage.Header (useHeader)
import Domain.Homepage.Home as Home
import Domain.Homepage.HowItWorks as HowItWorks
import Domain.Homepage.NotFound as NotFound
import Domain.Homepage.Route (Route)
import Domain.Homepage.Route as Route
import Domain.Homepage.Terms as Terms
import Domain.Homepage.Theme as Theme
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Tecton
  ( CSS
  , alignItems
  , backgroundColor
  , body
  , color
  , column
  , display
  , em
  , flex
  , flexDirection
  , flexGrow
  , fontFamily
  , gap
  , grid
  , height
  , html
  , margin
  , marginRight
  , minHeight
  , nil
  , pct
  , px
  , stretch
  , universal
  , vw
  , (:=)
  , (?)
  , (@+@)
  , (@-@)
  , (~)
  )
import Tecton.Halogen ((&.))
import Tecton.Rule as Rule
import Type.Proxy (Proxy(..))

block = ClassName "app" :: ClassName

headerWrapElement = ClassName "app__header" :: ClassName

mainElement = ClassName "app__main" :: ClassName

footerWrapElement = ClassName "app__footer" :: ClassName

verticalSpacerElement = ClassName "app__vspace" :: ClassName

css :: CSS
css = do
  html /\ body ? Rule.do
    height := pct 100
  body ? Rule.do
    backgroundColor := Theme.darkGray
    color := white
    fontFamily := Theme.roboto
    margin := nil ~ px 32
    marginRight := px 32 @-@ vw 100 @+@ pct 100
    display := flex
    flexDirection := column
    alignItems := stretch
  universal &. block ? Rule.do
    height := pct 100
    display := flex
    flexDirection := column
    gap := em 2
  universal &. mainElement ? Rule.do
    flexGrow := 1
    display := grid
  universal &. verticalSpacerElement ? Rule.do
    height := px 1
    minHeight := px 1

data Query a = Navigate (Maybe Route) a

type SimpleSlot = forall q. H.Slot q Void Unit

type Slots' a =
  ( home :: a
  , howItWorks :: a
  , terms :: a
  , notFound :: a
  )

type Slots = Slots' SimpleSlot

_home = Proxy :: Proxy "home"

_howItWorks = Proxy :: Proxy "howItWorks"

_terms = Proxy :: Proxy "terms"

_notFound = Proxy :: Proxy "notFound"

component :: forall i o m. MonadEffect m => H.Component Query i o m
component =
  Hooks.component \{ queryToken } _ -> Hooks.do
    route /\ routeId <- Hooks.useState Nothing
    header <- useHeader route
    footer <- useFooter route
    Hooks.useQuery queryToken \(Navigate r _) -> Hooks.put routeId r *> pure
      Nothing
    Hooks.pure $
      HH.div
        [ HP.class_ block ]
        [ HH.div [ HP.class_ verticalSpacerElement ] []
        , HH.div [ HP.class_ headerWrapElement ] [ header ]
        , HH.main
            [ HP.class_ mainElement ]
            [ case route of
                Just Route.Home ->
                  HH.slot_ _home unit Home.component unit
                Just Route.HowItWorks ->
                  HH.slot_ _howItWorks unit HowItWorks.component unit
                Just Route.Terms ->
                  HH.slot_ _terms unit Terms.component unit
                _ ->
                  HH.slot_ _notFound unit NotFound.component unit
            ]
        , HH.div [ HP.class_ footerWrapElement ] [ footer ]
        , HH.div [ HP.class_ verticalSpacerElement ] [ HH.text " " ]
        ]
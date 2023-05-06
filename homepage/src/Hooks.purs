module Domain.Homepage.Hooks
  ( UseMeasure
  , UseRefLabel
  , UseUnique
  , useMeasure
  , useRefLabel
  , useUnique
  ) where

import Prelude

import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple (curry, fst)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unique (Unique, hashUnique, newUnique)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (RefLabel(..))
import Halogen.Hooks
  ( class HookNewtype
  , type (<>)
  , Hook
  , HookType
  , Pure
  , UseEffect
  , UseState
  )
import Halogen.Hooks as Hooks
import Halogen.Subscription as HS
import Web.DOM.Element (DOMRect)
import Web.HTML.HTMLElement as HTMLElement
import Web.ResizeObserver (resizeObserver)
import Web.ResizeObserver as ResizeObserver

foreign import data UseUnique :: HookType

instance HookNewtype UseUnique (UseState (Maybe Unique) <> UseEffect <> Pure)

useUnique :: forall m. MonadEffect m => Hook m UseUnique (Maybe Unique)
useUnique =
  Hooks.wrap $
    Hooks.do
      unique /\ uniqueId <- Hooks.useState Nothing
      Hooks.useLifecycleEffect do
        uq <- liftEffect newUnique
        Hooks.put uniqueId $ Just uq
        pure Nothing
      Hooks.pure unique

foreign import data UseRefLabel :: HookType

instance HookNewtype UseRefLabel (UseUnique <> Pure)

useRefLabel
  :: forall m. MonadEffect m => Hook m UseRefLabel (Maybe RefLabel)
useRefLabel =
  Hooks.wrap $
    Hooks.do
      unique <- useUnique
      Hooks.pure $ RefLabel <<< show <<< hashUnique <$> unique

foreign import data UseMeasure :: HookType

instance
  HookNewtype UseMeasure (UseRefLabel <> UseState DOMRect <> UseEffect <> Pure)

useMeasure
  :: forall m. MonadEffect m => Hook m UseMeasure (Maybe RefLabel /\ DOMRect)
useMeasure =
  Hooks.wrap $
    Hooks.do
      refLabel <- useRefLabel
      rect /\ rectId <- Hooks.useState
        { top: 0.0
        , right: 0.0
        , bottom: 0.0
        , left: 0.0
        , width: 0.0
        , height: 0.0
        , x: 0.0
        , y: 0.0
        }
      Hooks.captures { refLabel }
        Hooks.useTickEffect
        do
          { emitter, listener } <- liftEffect HS.create
          obs <- liftEffect
            $ resizeObserver
            $ curry
            $ fst >>> traverse_
                \{ contentRect } ->
                  HS.notify listener $ Hooks.put rectId contentRect
          for_ refLabel $ Hooks.getHTMLElementRef >=>
            traverse_
              ( liftEffect <<< flip (flip ResizeObserver.observe {}) obs <<<
                  HTMLElement.toElement
              )
          subscription <- Hooks.subscribe emitter
          pure $ Just do
            Hooks.unsubscribe subscription
            liftEffect $ ResizeObserver.disconnect obs
      Hooks.pure $ refLabel /\ rect
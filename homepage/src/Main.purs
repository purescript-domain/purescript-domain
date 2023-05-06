module Domain.Homepage.Main
  ( main
  ) where

import Prelude

import Data.Maybe (Maybe(Just))
import Domain.Homepage.App as App
import Domain.Homepage.Route as Route
import Effect (Effect)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Routing.Hash (matchesWith)

main :: Effect Unit
main = do
  { emitter, listener } <- HS.create
  runHalogenAff do
    body <- awaitBody
    io <- runUI App.component unit body
    liftEffect do
      void $ HS.subscribe emitter $ runHalogenAff <<< io.query <<< H.mkTell <<<
        App.Navigate
      void $ matchesWith (Just <<< Route.parse) $ const $ HS.notify listener
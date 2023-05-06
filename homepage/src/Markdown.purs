module Domain.Homepage.Markdown
  ( UseMarkdown
  , css
  , useMarkdown
  ) where

import Prelude

import Data.Array (catMaybes, (:))
import Data.Maybe (Maybe(..))
import Domain.Homepage.Hooks (UseRefLabel, useRefLabel)
import Domain.Homepage.Link as Link
import Domain.Homepage.Theme as Theme
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks
  ( class HookNewtype
  , type (<>)
  , Hook
  , HookType
  , Pure
  , UseEffect
  )
import Halogen.Hooks as Hooks
import Tecton
  ( CSS
  , a
  , code
  , color
  , em
  , em'
  , fontFamily
  , fontStyle
  , lineHeight
  , margin
  , marginTop
  , nil
  , normal
  , universal
  , (:=)
  , (?)
  , (|*)
  , (|+)
  , (|>)
  )
import Tecton.Halogen ((&.))
import Tecton.Rule as Rule
import Web.DOM (Element)

block = ClassName "prose" :: ClassName

css :: CSS
css = do
  universal &. block ? Rule.do
    fontFamily := Theme.roboto
    lineHeight := 1.5
  universal &. block |* em' ? Rule.do
    fontStyle := normal
    color := Theme.gold
  universal &. block |* code ? Rule.do
    fontFamily := Theme.inconsolata
  universal &. block |* a # Link.css'
  universal &. block |> universal ? Rule.do
    margin := nil
  universal &. block |> universal |+ universal ? Rule.do
    marginTop := em 0.75

foreign import data UseMarkdown :: HookType

instance
  HookNewtype UseMarkdown
    (UseRefLabel <> UseEffect <> Pure)

useMarkdown
  :: forall m w i
   . MonadEffect m
  => String
  -> Hook m UseMarkdown (HH.HTML w i)
useMarkdown md =
  Hooks.wrap $
    Hooks.do
      containerRefLabel <- useRefLabel

      Hooks.captures
        {}
        Hooks.useTickEffect
        case containerRefLabel of
          Nothing ->
            pure Nothing
          Just rl ->
            Hooks.getRef rl >>=
              case _ of
                Nothing ->
                  pure Nothing
                Just el -> do
                  liftEffect $ parseMarkdown md >>= setInnerHTML el
                  pure $ Just $ liftEffect $ setInnerHTML el mempty

      Hooks.pure $
        HH.div
          (HP.class_ block : catMaybes [ HP.ref <$> containerRefLabel ])
          []

foreign import parseMarkdown :: String -> Effect String

foreign import setInnerHTML :: Element -> String -> Effect Unit
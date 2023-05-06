module Domain.Homepage.Terms
  ( component
  ) where

import Domain.Homepage.Article (useArticle)
import Domain.Homepage.Markdown (useMarkdown)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Hooks as Hooks

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  Hooks.component \_ _ -> Hooks.do
    content <- useMarkdown markdownContent
    useArticle content

foreign import markdownContent :: String
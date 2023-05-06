module Domain.Homepage.Article
  ( UseArticle
  , css
  , useArticle
  ) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (class HookNewtype, Hook, HookType, Pure)
import Halogen.Hooks as Hooks
import Tecton
  ( CSS
  , auto
  , margin
  , maxWidth
  , nil
  , pct
  , px
  , universal
  , width
  , (:=)
  , (?)
  , (~)
  )
import Tecton.Halogen ((&.))
import Tecton.Rule as Rule
import Web.HTML.Common (ClassName(..))

block = ClassName "article" :: ClassName

css :: CSS
css = do
  universal &. block ? Rule.do
    width := pct 100
    maxWidth := px 900
    margin := nil ~ auto

foreign import data UseArticle :: HookType

instance HookNewtype UseArticle Pure

useArticle :: forall m w i. HH.HTML w i -> Hook m UseArticle (HH.HTML w i)
useArticle =
  Hooks.wrap <<< Hooks.pure <<< HH.div [ HP.class_ block ] <<< pure
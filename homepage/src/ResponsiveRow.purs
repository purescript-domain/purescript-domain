module Domain.Homepage.ResponsiveRow
  ( UseResponsiveRow2
  , css
  , useResponsiveRow2
  ) where

import Prelude

import Data.Array (catMaybes, (:))
import Data.Tuple.Nested ((/\))
import Domain.Homepage.Hooks (UseMeasure, useMeasure)
import Effect.Class (class MonadEffect)
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (type (<>), Hook, Pure)
import Halogen.Hooks as Hooks
import Tecton
  ( CSS
  , alignItems
  , center
  , column
  , display
  , em
  , flex
  , flexDirection
  , gap
  , hidden
  , inlineBlock
  , justifyContent
  , nowrap
  , overflow
  , row
  , spaceBetween
  , universal
  , whiteSpace
  , (:=)
  , (?)
  )
import Tecton.Halogen ((&.))
import Tecton.Rule as Rule

block = ClassName "responsive-row" :: ClassName

blockNarrowModifier = ClassName "responsive-row--narrow" :: ClassName

cellElement = ClassName "responsive-row__cell" :: ClassName

css :: CSS
css = do
  universal &. block ? Rule.do
    display := flex
    flexDirection := row
    alignItems := center
    justifyContent := spaceBetween
    gap := em 1
    overflow := hidden
  universal &. blockNarrowModifier ? Rule.do
    flexDirection := column
  universal &. cellElement ? Rule.do
    display := inlineBlock
    whiteSpace := nowrap

type UseResponsiveRow2 = UseMeasure <> UseMeasure <> UseMeasure <> Pure

useResponsiveRow2
  :: forall m w i
   . MonadEffect m
  => HH.HTML w i
  -> HH.HTML w i
  -> Hook m (UseMeasure <> UseMeasure <> UseMeasure <> Pure) (HH.HTML w i)
useResponsiveRow2 a b = Hooks.do
  blockRefLabel /\ blockBox <- useMeasure
  cellARefLabel /\ cellABox <- useMeasure
  cellBRefLabel /\ cellBBox <- useMeasure
  Hooks.pure $
    HH.div
      ( HP.classes
          ( block :
              if cellABox.width + cellBBox.width >= blockBox.width then
                [ blockNarrowModifier ]
              else []
          )
          : catMaybes [ HP.ref <$> blockRefLabel ]
      )
      [ HH.div_
          [ HH.div
              (HP.class_ cellElement : catMaybes [ HP.ref <$> cellARefLabel ])
              [ a ]
          ]
      , HH.div_
          [ HH.div
              (HP.class_ cellElement : catMaybes [ HP.ref <$> cellBRefLabel ])
              [ b ]
          ]
      ]
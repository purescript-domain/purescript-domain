module Domain.Homepage.SearchBox
  ( UseSearchBox
  , css
  , useSearchBox
  ) where

import Prelude

import Color (black, white)
import Data.Array (catMaybes, elem, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Data.Unique (hashUnique)
import Domain.Homepage.Hooks
  ( UseMeasure
  , UseRefLabel
  , UseUnique
  , useMeasure
  , useRefLabel
  , useUnique
  )
import Domain.Homepage.Logo (UseLogo, useLogo)
import Domain.Homepage.Theme as Theme
import Domain.Sites (siteNames)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties (AutocompleteType(..))
import Halogen.HTML.Properties as HP
import Halogen.Hooks
  ( class HookNewtype
  , type (<>)
  , Hook
  , HookType
  , Pure
  , UseEffect
  , UseRef
  , useRef
  , useTickEffect
  )
import Halogen.Hooks as Hooks
import Tecton
  ( CSS
  , alignItems
  , backgroundColor
  , backgroundImage
  , bolder
  , borderWidth
  , boxShadow
  , center
  , color
  , deg
  , display
  , em
  , flex
  , flexGrow
  , flexWrap
  , focusWithin
  , fontFamily
  , fontSize
  , fontWeight
  , inlineBlock
  , justifyContent
  , lineHeight
  , linearGradient
  , margin
  , nil
  , none
  , outlineStyle
  , paddingBottom
  , paddingLeft
  , paddingRight
  , paddingTop
  , pct
  , px
  , stretch
  , transparent
  , universal
  , width
  , wrap
  , (&:)
  , (:=)
  , (?)
  , (~)
  )
import Tecton.Halogen ((&.))
import Tecton.Rule as Rule
import Web.DOM.Element as Element
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Event (preventDefault)
import Web.Event.EventTarget
  ( addEventListener
  , eventListener
  , removeEventListener
  )
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (focus)
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.HTMLInputElement as Input
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.UIEvent.InputEvent.EventTypes (input)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)

block = ClassName "search-box" :: ClassName

blockNarrowModifier = ClassName "search-box--narrow" :: ClassName

logoWrapElement = ClassName "search-box__logo-wrap" :: ClassName

blockNarrowModifierLogoWrapElement =
  ClassName "search-box--narrow__logo-wrap" :: ClassName

logoMeasureElement = ClassName "search-box__logo-measure" :: ClassName

slashElement = ClassName "search-box__slash" :: ClassName

blockNarrowModifierSlashElement =
  ClassName "search-box--narrow__slash" :: ClassName

inputElement = ClassName "search-box__input" :: ClassName

blockNarrowModifierInputElement =
  ClassName "search-box--narrow__input" :: ClassName

css :: CSS
css = do
  universal &. block ? Rule.do
    fontSize := em 0.75
    backgroundColor := Theme.darkerGray
    display := flex
    alignItems := stretch
  universal &. block &: focusWithin ? Rule.do
    boxShadow := Theme.gold ~ nil ~ nil ~ nil ~ px 1
  universal &. blockNarrowModifier ? Rule.do
    width := pct 100
    flexWrap := wrap
  universal &. logoWrapElement ? Rule.do
    backgroundColor := black
    display := flex
    alignItems := center
    justifyContent := center
    paddingLeft := em 1
  universal &. logoMeasureElement ? Rule.do
    display := inlineBlock
  universal &. slashElement ? Rule.do
    display := flex
    alignItems := center
    fontFamily := Theme.montserrat
    fontSize := em 2.5
    fontWeight := bolder
    color := Theme.gold
    backgroundImage :=
      linearGradient (deg 90) $ black ~ pct 50 /\ Theme.darkerGray ~ pct 50
    paddingLeft := em 0.25
    paddingRight := em 0.375
  universal &. blockNarrowModifierSlashElement ? Rule.do
    flexGrow := 1
    backgroundColor := black
    backgroundImage := none
  universal &. logoWrapElement
    /\ universal &. slashElement
    /\ universal &. inputElement
      ? Rule.do
          paddingTop := em 0.5
          paddingBottom := em 0.5
  universal &. inputElement ? Rule.do
    width := pct 100
    flexGrow := 1
    backgroundColor := transparent
    color := white
    borderWidth := nil
    outlineStyle := none
    fontFamily := Theme.montserrat
    fontSize := em 2.5
    margin := nil
    paddingLeft := nil
    paddingRight := em 0.5
    lineHeight := 1
  universal &. blockNarrowModifierInputElement ? Rule.do
    paddingLeft := em 0.5

foreign import data UseSearchBox :: HookType

instance
  HookNewtype UseSearchBox
    ( UseLogo <> UseUnique <> UseRef String <> UseMeasure <> UseMeasure
        <> UseRefLabel
        <> UseEffect
        <> Pure
    )

useSearchBox :: forall m w i. MonadEffect m => Hook m UseSearchBox (HH.HTML w i)
useSearchBox =
  Hooks.wrap $
    Hooks.do
      logo <- useLogo { link: false }

      datalistId <- map (\uq -> "searchboxlist_" <> show (hashUnique uq)) <$>
        useUnique

      _ /\ keypressRef <- useRef ""

      formRefLabel /\ formBox <- useMeasure
      logoMeasureRefLabel /\ logoBox <- useMeasure
      inputRefLabel <- useRefLabel

      Hooks.captures {} useTickEffect
        case inputRefLabel of
          Nothing ->
            pure Nothing
          Just inputRefLabel' -> do
            inputEl <- Hooks.getHTMLElementRef inputRefLabel'
            case inputEl >>= HTMLInputElement.fromHTMLElement of
              Nothing ->
                pure Nothing
              Just inputEl' -> do

                focusListener <-
                  liftEffect
                    $ eventListener
                    $ KeyboardEvent.fromEvent >>>
                        maybe (pure unit) \event ->
                          when (KeyboardEvent.key event == "/") do
                            hasFocus <-
                              Element.matches
                                (QuerySelector ":focus")
                                (HTMLInputElement.toElement inputEl')
                            when (not hasFocus) do
                              focus $ HTMLInputElement.toHTMLElement inputEl'
                              preventDefault $ KeyboardEvent.toEvent event
                docTarget <-
                  liftEffect
                    $ window >>= Window.document
                        >>> map HTMLDocument.toEventTarget
                liftEffect $ addEventListener keydown focusListener false
                  docTarget

                let
                  submit event = do
                    preventDefault event
                    siteName <- Input.value inputEl'
                    if siteName `elem` siteNames then
                      window
                        >>= Window.location
                        >>= Location.setHref
                          ("https://purescri.pt/" <> siteName)
                    else
                      window >>= Window.alert
                        ( "The site \"" <> siteName <>
                            "\" does not exist. Please check your spelling and try again."
                        )

                inputKeydownListener <-
                  liftEffect
                    $ eventListener
                    $ KeyboardEvent.fromEvent >>>
                        maybe
                          (Ref.write "" keypressRef)
                          ( \event ->
                              case KeyboardEvent.key event of
                                "Enter" ->
                                  submit $ KeyboardEvent.toEvent event
                                k ->
                                  Ref.write k keypressRef
                          )
                inputChangeListener <-
                  liftEffect
                    $ eventListener
                        \event -> do
                          key <- Ref.read keypressRef
                          when (key == "") $ submit event
                let inputElTarget = HTMLInputElement.toEventTarget inputEl'
                liftEffect do
                  addEventListener keydown inputKeydownListener false
                    inputElTarget
                  addEventListener input inputChangeListener false inputElTarget

                pure
                  $ Just
                  $ liftEffect do
                      removeEventListener keydown focusListener false docTarget
                      removeEventListener keydown inputKeydownListener false
                        inputElTarget
                      removeEventListener input inputChangeListener false
                        inputElTarget

      let narrow = formBox.width < 2.5 * logoBox.width

      Hooks.pure $
        HH.label
          ( [ HP.classes $ block :
                if narrow then [ blockNarrowModifier ] else []
            ] <> catMaybes [ HP.ref <$> formRefLabel ]
          )
          [ HH.div
              [ HP.classes $ logoWrapElement :
                  if narrow then [ blockNarrowModifierLogoWrapElement ] else []
              ]
              [ HH.div
                  ( HP.class_ logoMeasureElement : catMaybes
                      [ HP.ref <$> logoMeasureRefLabel ]
                  )
                  [ logo ]
              ]
          , HH.div
              [ HP.classes $ slashElement :
                  if narrow then [ blockNarrowModifierSlashElement ] else []
              ]
              [ HH.text "/" ]
          , HH.input $
              [ HP.type_ HP.InputText
              , HP.classes $ inputElement :
                  if narrow then [ blockNarrowModifierInputElement ] else []
              , HP.autofocus true
              , HP.autocomplete AutocompleteOff
              ] <> catMaybes
                [ HP.ref <$> inputRefLabel, HP.list <$> datalistId ]
          , HH.datalist
              (catMaybes [ HP.id <$> datalistId ])
              $ (\name -> HH.option_ [ HH.text name ]) <$> siteNames
          ]
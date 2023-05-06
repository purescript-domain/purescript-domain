module Domain.Site where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , decodeJson
  , encodeJson
  )
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Mode = Mask | Redirect

derive instance Generic Mode _

instance Show Mode where
  show = genericShow

data Site
  = Site Mode String
  | DisabledSite

derive instance Generic Site _

instance Show Site where
  show = genericShow

instance DecodeJson Site where
  decodeJson json =
    ( (\({ redirect } :: { redirect :: String }) -> Site Redirect redirect) <$>
        decodeJson json
    )
      <|>
        ( (\({ mask } :: { mask :: String }) -> Site Mask mask) <$> decodeJson
            json
        )
      <|>
        ( (\(_ :: { disable :: Unit }) -> DisabledSite) <$> decodeJson
            json
        )

instance EncodeJson Site where
  encodeJson (Site Redirect redirect) = encodeJson { redirect }
  encodeJson (Site Mask mask) = encodeJson { mask }
  encodeJson DisabledSite = encodeJson { disable: unit }
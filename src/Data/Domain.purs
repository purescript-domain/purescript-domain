module PurescriPT.Data.Domain where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Decode.Decoders (decodeJArray)
import Data.Array ((!!))
import Data.Either (note)
import Data.Tuple.Nested (type (/\), (/\))

type Name = String

type Redirect = String

newtype Domain = Domain (Name /\ Redirect /\ { proxy :: Boolean })

mkDomain :: Name -> Redirect -> { proxy :: Boolean } -> Domain
mkDomain n r o = Domain $ n /\ r /\ o

instance Eq Domain where
  eq (Domain a) (Domain b) = a == b

instance Show Domain where
  show (Domain (n /\ r /\ o)) =
    "(Domain " <> show n <> " " <> show r <> " " <> show o <> ")"

instance DecodeJson Domain where
  decodeJson json = do
    arr <- decodeJArray json
    let mem i = note (AtIndex i MissingValue) $ arr !! i
    n /\ r <- (/\) <$> (mem 0 >>= decodeJson) <*> (mem 1 >>= decodeJson)
    o <- (mem 2 >>= decodeJson) <|> pure { proxy: true }
    pure $ Domain $ n /\ r /\ o

name :: Domain -> Name
name (Domain (x /\ _)) = x

redirect :: Domain -> Redirect
redirect (Domain (_ /\ x /\ _)) = x

proxy :: Domain -> Boolean
proxy (Domain (_ /\ _ /\ options)) = options.proxy

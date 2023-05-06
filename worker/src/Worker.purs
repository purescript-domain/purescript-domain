module Domain.Worker (fetch, fetch') where

import Prelude

import Cloudflare.Workers.Request (Request)
import Cloudflare.Workers.Response (Response, mkResponse)
import Data.Argonaut (Json, decodeJson, printJsonDecodeError)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), either, fromRight, hush)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Regex (match, test) as Regex
import Data.String.Regex (regex)
import Data.String.Regex.Flags (ignoreCase, noFlags) as Regex
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Domain.Site (Mode(..), Site(..))
import Domain.Sites as Sites
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)
import Effect.Uncurried (EffectFn2, mkEffectFn2)
import Foreign.Object (Object)
import Foreign.Object as Object
import Promise.Aff (Promise)
import Promise.Aff as Promise
import Type.Proxy (Proxy(..))
import TypedEnv (printEnvError)
import TypedEnv as TypedEnv

fetch :: EffectFn2 Request (Object String) (Promise Response)
fetch = mkEffectFn2 \req env -> Promise.fromAff $ fetch' Sites.sites
  defaultFetchUpstream
  req
  env

fetch'
  :: Object Json
  -> (String -> Aff Response)
  -> Request
  -> Object String
  -> Aff Response
fetch' sites fetchUpstream req env = do
  tenv <-
    either (throwError <<< error <<< printEnvError) pure $
      TypedEnv.fromEnv (Proxy :: _ { "DOMAIN_SITE" :: String }) env
  case
    String.dropWhile (_ == String.codePointFromChar '/') <$>
      join
        ( hush (regex "^https?://[^/]+(.*)" Regex.noFlags)
            >>= flip Regex.match req.url
            >>= flip NEA.index 1
        )
    of
    Nothing ->
      throwError $ error $ "Unable to get pathname from URL " <> req.url
    Just pathname ->
      case
        Array.uncons (String.split (Pattern "/") pathname) >>=
          \{ head, tail } -> Tuple (String.joinWith "/" tail) <$> Object.lookup
            head
            sites
        of
        Nothing ->
          mask fetchUpstream req tenv."DOMAIN_SITE" pathname
        Just (pathname' /\ siteJson) ->
          case decodeJson siteJson of
            Left err ->
              throwError $ error $ "Invalid site JSON: " <> printJsonDecodeError
                err
            Right (Site Redirect url) ->
              redirect url pathname'
            Right (Site Mask url) ->
              mask fetchUpstream req url pathname'
            Right DisabledSite ->
              pure $ mkResponse "The requested site has been disabled."
                { status: 404, statusText: "Not Found" }

redirect :: String -> String -> Aff Response
redirect url pathname =
  let
    headers = Object.empty # Object.insert "Location"
      (url <> if pathname == mempty then mempty else ("/" <> pathname))
    status = 301
    statusText = "Moved Permanently"
  in
    pure $ mkResponse "" { status, statusText, headers }

mask :: (String -> Aff Response) -> Request -> String -> String -> Aff Response
mask fetchUpstream req url pathname =
  if
    Array.last (String.toCodePointArray req.url) /= Just
      (String.codePointFromChar '/')
      && not
        ( fromRight false $ flip Regex.test req.url <$> regex "\\.[a-z]+$"
            Regex.ignoreCase
        ) then pure $ mkResponse ""
    { status: 301
    , statusText: "Moved Permanently"
    , headers: Object.empty # Object.insert "Location" (req.url <> "/")
    }
  else fetchUpstream $ url <> "/" <> pathname

defaultFetchUpstream :: String -> Aff Response
defaultFetchUpstream = Promise.toAffE <<< fetchUpstreamImpl

foreign import fetchUpstreamImpl :: String -> Effect (Promise Response)
module PurescriPT.Cloudflare where

import Prelude

import Data.Argonaut (decodeJson, encodeJson, parseJson, printJsonDecodeError)
import Data.Argonaut as JSON
import Data.Array.Partial as Array
import Data.Either (Either(..))
import Data.String.Common as String
import Data.String.Pattern (Pattern(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Milkis as M
import Milkis.Impl.Node (nodeFetch)
import Partial.Unsafe (unsafePartial)
import PurescriPT.Data.Domain (Domain, mkDomain)
import PurescriPT.Data.Domain as Domain

fetch :: M.Fetch
fetch = M.fetch nodeFetch

type Credentials =
  { zoneId :: String
  , authEmail :: String
  , authKey :: String
  }

type Id = String

listDomains :: Credentials -> Aff (Either String (Array (Id /\ Domain)))
listDomains { authEmail, authKey, zoneId } = do
  res <- fetch
           (M.URL $ "https://api.cloudflare.com/client/v4/zones/" <> zoneId <> "/dns_records?per_page=5000&type=CNAME")
           { method: M.getMethod
           , headers:
               M.makeHeaders
               { "x-auth-email": authEmail
               , "x-auth-key": authKey
               }
           }
  text <- M.text res
  case M.statusCode res of
    200 ->
      case parseJson text >>= decodeJson of
        Left error ->
          pure $ Left $ "Unexpected response format. " <> printJsonDecodeError error
        Right
          ({ result: rs }
             :: { result
                    :: Array
                         { id :: String
                         , name :: String
                         , content :: String
                         , proxied :: Boolean
                         }
                }
          ) ->
          pure
            $ Right
            $ rs
              <#>
              \r ->
                let
                  name = unsafePartial $ Array.head $ String.split (Pattern ".") r.name
                in
                  r.id /\ mkDomain name r.content { proxy: r.proxied }
    invalid ->
      pure $ Left $ "Invalid " <> show invalid <> " response: " <> text

addDomain :: Credentials -> Domain -> Aff (Either String Domain)
addDomain { authEmail, authKey, zoneId } domain = do
  res <- fetch
           (M.URL $ "https://api.cloudflare.com/client/v4/zones/" <> zoneId <> "/dns_records")
           { method: M.postMethod
           , headers:
               M.makeHeaders
               { "x-auth-email": authEmail
               , "x-auth-key": authKey
               , "content-type": "application/json"
               }
           , body:
               JSON.stringify $
                 encodeJson
                   { type: "CNAME"
                   , name: Domain.name domain <> ".purescri.pt"
                   , content: Domain.redirect domain
                   , ttl: 1800
                   , proxied: Domain.proxy domain
                   }
           }
  case M.statusCode res of
    200 ->
      pure $ Right domain
    invalid -> do
      text <- M.text res
      pure $ Left $ "Invalid " <> show invalid <> " response: " <> text

updateDomain :: Credentials -> Id -> Domain -> Aff (Either String (Id /\ Domain))
updateDomain { authEmail, authKey, zoneId } id domain = do
  res <- fetch
           (M.URL $ "https://api.cloudflare.com/client/v4/zones/" <> zoneId <> "/dns_records/" <> id)
           { method: M.patchMethod
           , headers:
               M.makeHeaders
               { "x-auth-email": authEmail
               , "x-auth-key": authKey
               , "content-type": "application/json"
               }
           , body:
               JSON.stringify $
                 encodeJson
                   { type: "CNAME"
                   , name: Domain.name domain <> ".purescri.pt"
                   , content: Domain.redirect domain
                   , proxied: Domain.proxy domain
                   }
           }
  case M.statusCode res of
    200 ->
      pure $ Right $ id /\ domain
    invalid -> do
      text <- M.text res
      pure $ Left $ "Invalid " <> show invalid <> " response: " <> text

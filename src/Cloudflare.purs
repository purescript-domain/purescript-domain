module PurescriPT.Cloudflare where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut (decodeJson, encodeJson, parseJson, printJsonDecodeError)
import Data.Argonaut as JSON
import Data.Array.Partial as Array
import Data.Either (Either(..))
import Data.String.Common as String
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign.Object (Object)
import Foreign.Object as Object
import Milkis as M
import Milkis.Impl.Node (nodeFetch)
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Nub, class Union)
import PurescriPT.Data.Domain (Domain, mkDomain)
import PurescriPT.Data.Domain as Domain
import Record as Record
import Type.Prelude (Proxy(..))

type Credentials r =
  { zoneId :: String
  , authEmail :: String
  , authKey :: String
  | r
  }

type Id = String

type Client m =
  { listDomains :: m (Array (Id /\ Domain))
  , addDomain :: Domain -> m Domain
  , updateDomain :: Id -> Domain -> m (Id /\ Domain)
  , deleteDomain :: Id -> Domain -> m (Id /\ Domain)
  }

mkClient
  :: forall r m
   . MonadError String m
  => MonadAff m
  => Credentials r
  -> Client m
mkClient { authEmail, authKey, zoneId } =
  let

    fetchArgs
      :: forall s a b
       . Union a (headers :: Object String) b
      => Nub b (headers :: Object String | s)
      => String
      -> Record a
      -> M.URL /\ { headers :: Object String | s }
    fetchArgs url opts =
      Tuple
        (M.URL $ "https://api.cloudflare.com/client/v4/zones/" <> zoneId <> "/dns_records" <> url)
        $ Record.modify (Proxy :: Proxy "headers") (Object.insert "x-auth-email" authEmail <<< Object.insert "x-auth-key" authKey)
        $ Record.merge opts { headers: Object.empty :: Object String }

    listDomains = do
      res <- liftAff $ uncurry (M.fetch nodeFetch) $ fetchArgs "?per_page=5000&type=CNAME" { method: M.getMethod }
      text <- liftAff $ M.text res
      case M.statusCode res of
        200 ->
          case parseJson text >>= decodeJson of
            Left error ->
              throwError $ "Unexpected response format. " <> printJsonDecodeError error
            Right
              ( { result: rs }
                  ::
                       { result ::
                           Array
                             { id :: String
                             , name :: String
                             , content :: String
                             , proxied :: Boolean
                             }
                       }
              ) ->
              pure
                $ rs
                    <#>
                      \r ->
                        let
                          name = unsafePartial $ Array.head $ String.split (Pattern ".") r.name
                        in
                          r.id /\ mkDomain name r.content { proxy: r.proxied }
        invalid ->
          throwError $ "Invalid " <> show invalid <> " response: " <> text

    addDomain domain = do
      res <- liftAff
        $ uncurry (M.fetch nodeFetch)
        $ fetchArgs
            ""
            { method: M.postMethod
            , headers: M.makeHeaders { "content-type": "application/json" }
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
          pure domain
        invalid -> do
          text <- liftAff $ M.text res
          throwError $ "Invalid " <> show invalid <> " response: " <> text

    updateDomain id domain = do
      res <- liftAff
        $ uncurry (M.fetch nodeFetch)
        $ fetchArgs
            ("/" <> id)
            { method: M.patchMethod
            , headers: M.makeHeaders { "content-type": "application/json" }
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
          pure $ id /\ domain
        invalid -> do
          text <- liftAff $ M.text res
          throwError $ "Invalid " <> show invalid <> " response: " <> text

    deleteDomain id domain = do
      res <- liftAff
        $ uncurry (M.fetch nodeFetch)
        $ fetchArgs ("/" <> id) { method: M.deleteMethod }
      case M.statusCode res of
        200 ->
          pure $ id /\ domain
        invalid -> do
          text <- liftAff $ M.text res
          throwError $ "Invalid " <> show invalid <> " response: " <> text

  in
    { listDomains, addDomain, updateDomain, deleteDomain }

module Domains.DNS.Cloudflare where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut (JsonDecodeError, decodeJson, encodeJson, parseJson, printJsonDecodeError)
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
import Domains.DNS.Domain (Domain, mkDomain)
import Domains.DNS.Domain as Domain
import Record as Record
import Type.Prelude (Proxy(..))

type Credentials r =
  { zoneId :: String
  , authEmail :: String
  , authKey :: String
  | r
  }

type Id = String

data Operation = ListOp | AddOp Domain | UpdateOp Id Domain | DeleteOp Id

data ErrorType
  = UnexpectedStatus Int String
  | UnexpectedContent JsonDecodeError String

newtype Error = Error (Operation /\ ErrorType)

printError :: Error -> String
printError (Error (op /\ ty)) =
  let
    general =
      case op of
        ListOp ->
          "Failed to list domains"
        AddOp domain ->
          "Failed to add " <> show domain
        UpdateOp id new ->
          "Failed to update domain with id " <> id <> " to " <> show new
        DeleteOp id ->
          "Failed to delete domain with id " <> id
    details =
      case ty of
        UnexpectedStatus status content ->
          "Expected a 200 status, but received " <> show status <> " with the following content: " <> content
        UnexpectedContent decodeError content ->
          "An error occurred while decoding the response body. " <> printJsonDecodeError decodeError <> " Content received: " <> content
  in
    general <> ". " <> details

type Client m =
  { listDomains :: m (Array (Id /\ Domain))
  , addDomain :: Domain -> m Domain
  , updateDomain :: Id -> Domain -> m (Id /\ Domain)
  , deleteDomain :: Id -> m Id
  }

mkClient
  :: forall r m
   . MonadError Error m
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
              throwError $ Error $ ListOp /\ UnexpectedContent error text
            Right
              ( { result: rs }
                  :: { result ::
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
          throwError $ Error $ ListOp /\ UnexpectedStatus invalid text

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
          throwError $ Error $ AddOp domain /\ UnexpectedStatus invalid text

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
          throwError $ Error $ UpdateOp id domain /\ UnexpectedStatus invalid text

    deleteDomain id = do
      res <- liftAff
        $ uncurry (M.fetch nodeFetch)
        $ fetchArgs ("/" <> id) { method: M.deleteMethod }
      case M.statusCode res of
        200 ->
          pure id
        invalid -> do
          text <- liftAff $ M.text res
          throwError $ Error $ DeleteOp id /\ UnexpectedStatus invalid text

  in
    { listDomains, addDomain, updateDomain, deleteDomain }

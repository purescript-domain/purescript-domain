module Main where

import Prelude

import Data.Argonaut (JsonDecodeError, decodeJson, parseJson, printJsonDecodeError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (lookup)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Dotenv as Dotenv
import Effect (Effect)
import Effect.Aff (error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import PurescriPT.Cloudflare as Cloudflare
import PurescriPT.Data.Domain (Domain)
import PurescriPT.Data.Domain as Domain

main :: Effect Unit
main = launchAff_ do
  env <- Dotenv.loadFile

  let
    maybeCredentials =
      (\authEmail authKey zoneId -> { authEmail, authKey, zoneId })
      <$> join (lookup "CLOUDFLARE_AUTH_EMAIL" env)
      <*> join (lookup "CLOUDFLARE_AUTH_KEY" env)
      <*> join (lookup "CLOUDFLARE_ZONE_ID" env)

  case maybeCredentials of
    Just credentials -> do
      domainsText <- readTextFile UTF8 "./domains.json"
      case decodeDomains domainsText of
        Left e ->
          throwError $ error $ "Unable to parse domains.json: " <> printJsonDecodeError e
        Right domains -> do
          eitherExistingRecords <- Cloudflare.listDomains credentials
          case eitherExistingRecords of
            Right existingRecords -> do
              traverse_
                ( uncurry (Cloudflare.updateDomain credentials)
                    >=> liftEffect <<< case _ of
                      Left e ->
                        Console.error $ "Failed update: " <> e
                      Right (id /\ domain) ->
                        Console.info $ "Successful update: " <> show domain <> " with id " <> id
                )
                $ Array.catMaybes
                $ existingRecords
                    <#> \(id /\ domain) ->
                      case Array.find (\x -> Domain.name x == Domain.name domain && x /= domain) domains of
                        Just updatedDomain ->
                          Just $ id /\ updatedDomain
                        _ ->
                          Nothing
              traverse_
                ( Cloudflare.addDomain credentials
                    >=> liftEffect <<< case _ of
                      Left e ->
                        Console.error $ "Failed add: " <> e
                      Right domain ->
                        Console.info $ "Successful add: " <> show domain
                )
                $ Array.catMaybes
                $ domains
                    <#> \domain ->
                      case Array.find (\(_ /\ x) -> Domain.name x == Domain.name domain) existingRecords of
                        Nothing ->
                          Just domain
                        _ ->
                          Nothing

            Left e ->
              throwError $ error $ "Unable to get existing records: " <> e

    Nothing ->
      throwError $ error "Unable to get configuration."

  where

    decodeDomains :: String -> Either JsonDecodeError (Array Domain)
    decodeDomains = parseJson >=> decodeJson >=> traverse decodeJson

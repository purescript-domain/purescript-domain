module Main where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Except (runExceptT)
import Data.Argonaut (JsonDecodeError, decodeJson, parseJson, printJsonDecodeError)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foldable (lookup)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_, traverse, traverse_)
import Data.Tuple.Nested ((/\))
import Dotenv as Dotenv
import Effect (Effect)
import Effect.Aff (error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import PurescriPT.Cloudflare as CF
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
    Nothing ->
      throwError $ error "Unable to get configuration."
    Just credentials -> do
      domainsText <- readTextFile UTF8 "./domains.json"
      case decodeDomains domainsText of
        Left e ->
          throwError $ error $ "Unable to parse domains.json: " <> printJsonDecodeError e
        Right domains ->
          let
            { listDomains
            , addDomain
            , updateDomain
            , deleteDomain
            } = CF.mkClient credentials
          in
            runExceptT do

              existingRecords <- listDomains

              for_ existingRecords \(id /\ domain) ->
                let
                  matchingRecord = Array.find (\x -> Domain.name x == Domain.name domain) domains
                in
                  case matchingRecord of
                    Nothing -> do
                      result <- try $ deleteDomain id domain
                      liftEffect $ case result of
                        Left e ->
                          Console.error $ "Failed delete: " <> e
                        Right _ ->
                          Console.info $ "Successful delete: " <> show domain <> " with id " <> id
                    Just record ->
                      when (domain /= record) do
                        result <- try $ updateDomain id record
                        liftEffect $ case result of
                          Left e ->
                            Console.error $ "Failed update: " <> e
                          Right _ ->
                            Console.info $ "Successful update: " <> show domain <> " with id " <> id

              traverse_
                ( addDomain >>> try
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
              >>=
                either
                  (throwError <<< error)
                  (\_ -> liftEffect $ Console.log "Done!")

  where

  decodeDomains :: String -> Either JsonDecodeError (Array Domain)
  decodeDomains = parseJson >=> decodeJson >=> traverse decodeJson

module PurescriPT.DNS.Main where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Except (runExceptT)
import Data.Argonaut (JsonDecodeError, decodeJson, parseJson, printJsonDecodeError)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), isNothing)
import Data.Traversable (for_, traverse, traverse_)
import Data.Tuple.Nested ((/\))
import Dotenv as Dotenv
import Effect (Effect)
import Effect.Aff (error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Foreign.Object as Object
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Process (getEnv)
import PurescriPT.DNS.Cloudflare as CF
import PurescriPT.DNS.Domain (Domain)
import PurescriPT.DNS.Domain as Domain

main :: Effect Unit
main = launchAff_ do
  _ <- Dotenv.loadFile

  env <- liftEffect getEnv

  let
    maybeCredentials =
      (\authEmail authKey zoneId -> { authEmail, authKey, zoneId })
        <$> Object.lookup "CLOUDFLARE_AUTH_EMAIL" env
        <*> Object.lookup "CLOUDFLARE_AUTH_KEY" env
        <*> Object.lookup "CLOUDFLARE_ZONE_ID" env

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
                      result <- try $ deleteDomain id
                      liftEffect $ case result of
                        Left e ->
                          Console.error $ CF.printError e
                        Right _ ->
                          Console.info $ "Deleted: " <> show domain <> " with id " <> id
                    Just record ->
                      when (domain /= record) do
                        result <- try $ updateDomain id record
                        liftEffect $ case result of
                          Left e ->
                            Console.error $ CF.printError e
                          Right _ ->
                            Console.info $ "Updated: " <> show domain <> " with id " <> id

              traverse_
                ( addDomain >>> try
                    >=> liftEffect <<< case _ of
                      Left e ->
                        Console.error $ CF.printError e
                      Right domain ->
                        Console.info $ "Added: " <> show domain
                )
                $ Array.catMaybes
                $ domains
                    <#> \domain ->
                      if isNothing $ Array.find (\(_ /\ x) -> Domain.name x == Domain.name domain) existingRecords then Just domain
                      else Nothing
              >>=
                either
                  (throwError <<< error <<< CF.printError)
                  (\_ -> liftEffect $ Console.log "Done!")

  where

  decodeDomains :: String -> Either JsonDecodeError (Array Domain)
  decodeDomains = parseJson >=> decodeJson >=> traverse decodeJson

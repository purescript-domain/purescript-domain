module Domains.DNS.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (try)
import Control.Monad.Except (runExcept, runExceptT)
import Data.Argonaut (Json, JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Argonaut.Decode.Decoders (decodeForeignObject)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), isNothing)
import Data.Traversable (for_, traverse_)
import Data.Tuple.Nested ((/\))
import Data.YAML.Foreign.Decode (parseYAMLToJson)
import Domains.DNS.Cloudflare as CF
import Domains.DNS.Domain (Domain, mkDomain)
import Domains.DNS.Domain as Domain
import Dotenv as Dotenv
import Effect (Effect)
import Effect.Aff (error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Foreign (renderForeignError)
import Foreign.Object as Object
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Process (getEnv)

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
      domainsText <- readTextFile UTF8 "./domains.yml"
      case runExcept $ parseYAMLToJson domainsText of
        Left multiple ->
          throwError
            $ error
            $ foldl
                (\acc err -> acc <> "\n* " <> renderForeignError err)
                "Failed to parse domains.yml:"
                multiple
        Right domainsJson ->
          case decodeDomains domainsJson of
            Left e ->
              throwError $ error $ "Failed to parse domains.yml: " <> printJsonDecodeError e
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

  decodeDomains :: Json -> Either JsonDecodeError (Array Domain)
  decodeDomains json = do
    domains <-
      decodeForeignObject
        ( \value ->
            ((\redirect -> { redirect, proxy: true }) <$> decodeJson value)
              <|>
                decodeJson value
        )
        json
    pure $
      (\(name /\ { redirect, proxy }) -> mkDomain name redirect { proxy })
        <$>
          Object.toUnfoldable domains

module Domain.Configuration (ReadSitesError, readSites, printReadSitesError) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Argonaut (JsonDecodeError, printJsonDecodeError)
import Data.Argonaut (decodeJson)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Map as Map
import Data.YAML.Foreign.Decode (parseYAMLToJson)
import Domain.Site (Site)
import Domain.Site as Site
import Effect.Aff (Aff, attempt)
import Effect.Exception (Error)
import Effect.Exception as Error
import Foreign (ForeignError, renderForeignError)
import Foreign.Object (Object)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

data ReadSitesError
  = ReadFromConfigurationFileError Error
  | ParseConfigurationYAMLError (NonEmptyList ForeignError)
  | DecodeConfigurationError JsonDecodeError

readSites :: Aff (Either ReadSitesError (Object Site))
readSites = do
  content <- attempt $ readTextFile UTF8 "sites.yaml"
  case content of
    Left err ->
      pure $ Left $ ReadFromConfigurationFileError err
    Right sitesContent ->
      case runExcept $ parseYAMLToJson sitesContent of
        Left yamlErrors ->
          pure $ Left $ ParseConfigurationYAMLError yamlErrors
        Right json ->
          case decodeJson json of
            Left jsonError ->
              pure $ Left $ DecodeConfigurationError jsonError
            Right sites ->
              pure $ Right sites

printReadSitesError :: ReadSitesError -> String
printReadSitesError =
  case _ of
    ReadFromConfigurationFileError err ->
      "Failed to read configuration file: " <> Error.message err
    ParseConfigurationYAMLError xs | NEL.length xs == 1 ->
      "YAML parsing error: " <> (renderForeignError $ NEL.head xs)
    ParseConfigurationYAMLError xs | otherwise ->
      foldl
        (\b a -> b <> "\n* " <> renderForeignError a)
        "YAML parsing errors:"
        xs
    DecodeConfigurationError err ->
      "Failed to decode configuration: " <> printJsonDecodeError err
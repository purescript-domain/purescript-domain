module Domain.Registry
  ( FetchPackagesError
  , fetchPackages
  , printFetchPackagesError
  , packageMetadataToSite
  ) where

import Prelude

import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (bimap, lmap)
import Data.Codec.Argonaut (decode, printJsonDecodeError)
import Data.Either (Either(..))
import Data.Filterable (partitionMap)
import Data.Foldable (foldl)
import Data.List (List(Nil))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import Domain.Site (Mode(Redirect), Site(..))
import Effect.Aff (Aff, Error, attempt)
import Effect.Aff as Error
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign.Object (Object)
import Foreign.Object as Object
import Registry.Location (Location(..))
import Registry.Metadata (Metadata(..))
import Registry.Metadata as Metadata
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName

foreign import fetchPackageEntries :: EffectFnAff (Object String)

data PackageError
  = InvalidPackageName String
  | InvalidPackageMetadata PackageName String

data FetchPackagesError
  = FetchFromRegistryError Error
  | InvalidPackageData (List PackageError)

fetchPackages
  :: Aff (Either FetchPackagesError (List (PackageName /\ Metadata)))
fetchPackages = do
  packageEntries <- attempt $ fromEffectFnAff fetchPackageEntries
  case packageEntries of
    Left err ->
      pure $ Left $ FetchFromRegistryError err
    Right entries ->
      let
        decodedEntries =
          Object.toUnfoldable entries <#>
            ( \(name /\ content) ->
                case
                  jsonParser ("\"" <> name <> "\"") >>= decode PackageName.codec
                    >>> lmap
                      printJsonDecodeError
                  of
                  Left _ ->
                    Left $ InvalidPackageName name
                  Right packageName ->
                    bimap
                      (const $ InvalidPackageMetadata packageName content)
                      (\metadata -> packageName /\ metadata)
                      $ jsonParser content >>= decode Metadata.codec >>> lmap
                          printJsonDecodeError
            )
      in
        case partitionMap identity decodedEntries of
          { left: Nil, right: packages } ->
            pure $ Right packages
          { left: packageErrors, right: _ } ->
            pure $ Left $ InvalidPackageData packageErrors

printFetchPackagesError :: FetchPackagesError -> String
printFetchPackagesError =
  case _ of
    FetchFromRegistryError err ->
      "An error occurred while fetching package entries from the registry: " <>
        Error.message err
    InvalidPackageData errs | List.length errs == 0 ->
      "Invalid package data."
    InvalidPackageData errs | List.length errs == 1 ->
      "Invalid package data: " <> fromMaybe ""
        (printPackageError <$> List.head errs)
    InvalidPackageData errs | otherwise ->
      foldl (\b a -> b <> "\n* " <> printPackageError a) "Invalid package data:"
        errs
  where
  printPackageError =
    case _ of
      InvalidPackageName name ->
        "\"" <> name <> "\" is not a valid package name."
      InvalidPackageMetadata packageName content ->
        "Package \"" <> PackageName.print packageName
          <> "\" had invalid content:\n\n"
          <> content
          <> "\n"

-- Note / TODO: This runs in `Aff` because when `subdir` support is added it will
-- be necessary to fetch the default branch name from GitHub.
packageMetadataToSite :: Metadata -> Aff (Maybe Site)
packageMetadataToSite (Metadata { location }) =
  pure $
    case location of
      GitHub { owner, repo } ->
        pure $ Site Redirect $ "https://github.com/" <> owner <> "/" <> repo
      _ ->
        Nothing
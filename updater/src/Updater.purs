module Domain.Updater where

import Prelude

import Control.Apply (lift2)
import Data.Argonaut (encodeJson, stringify)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List (catMaybes)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Domain.Configuration (printReadSitesError, readSites)
import Domain.Registry
  ( fetchPackages
  , packageMetadataToSite
  , printFetchPackagesError
  )
import Domain.Site as Site
import Effect (Effect)
import Effect.Aff (error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign.Object as Object
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Node.Process (argv)
import Registry.PackageName as PackageName

main :: Effect Unit
main = do
  args <- argv
  let
    outputPath = Array.findIndex (_ == "-o") args >>= (_ + 1) >>>
      Array.index args
  launchAff_ $
    ( lift2 Tuple
        <$> (lmap printReadSitesError <$> readSites)
        <*> (lmap printFetchPackagesError <$> fetchPackages)
    ) >>=
      case _ of
        Left msg ->
          throwError $ error msg
        Right (sites /\ packages) -> do
          packagesAsSites :: Object.Object Site.Site <-
            Object.fromFoldable <<< catMaybes <$>
              for packages \(name /\ metadata) ->
                map (Tuple (PackageName.print name)) <$>
                  packageMetadataToSite metadata
          let
            allSites = Object.union sites packagesAsSites
            source =
              "export var sites = "
                <>
                  ( stringify $ encodeJson $ Object.fromFoldableWithIndex
                      allSites
                  )
                <> ";\n\nexport var siteNames = "
                <>
                  ( stringify $ encodeJson
                      (Array.fromFoldable $ Object.keys allSites)
                  )
                <> ";"
          case outputPath of
            Just outputPath' -> do
              writeTextFile UTF8 outputPath' source
              liftEffect $ log $ "Wrote configuration to " <> outputPath'
            Nothing ->
              liftEffect $ log source
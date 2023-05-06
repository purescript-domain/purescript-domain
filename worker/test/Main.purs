module Test.Worker where

import Prelude

import Cloudflare.Workers.Response (mkResponse)
import Cloudflare.Workers.Response as Response
import Data.Argonaut (encodeJson)
import Data.Maybe (Maybe(..))
import Domain.Site (Mode(..), Site(..))
import Domain.Worker as Worker
import Effect (Effect)
import Effect.Aff (launchAff_)
import Foreign.Object (insert)
import Foreign.Object as Object
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_ $ runSpec [ consoleReporter ] do
    let

      halogenUrl = "https://github.com/purescript-halogen/purescript-halogen"

      tectonUrl = "https://nsaunders.github.io/purescript-tecton"

      sites =
        Object.empty
          # insert "halogen" (encodeJson $ Site Redirect halogenUrl)
          # insert "tecton" (encodeJson $ Site Mask tectonUrl)
          # insert "chosen" (encodeJson $ DisabledSite)

      fetchUpstream url =
        pure $
          mkResponse ""
            { headers: Object.empty # insert "x-original-url" url
            }

      shouldMask res url = do
        Response.status res `shouldEqual` 200
        Object.lookup "x-original-url" (Response.headers res)
          `shouldEqual` Just url

      shouldRedirectTo res url = do
        Response.status res `shouldEqual` 301
        Response.statusText res `shouldEqual` "Moved Permanently"
        Object.lookup "Location" (Response.headers res) `shouldEqual` Just url

      shouldBeDisabled res = do
        Response.status res `shouldEqual` 404
        Response.statusText res `shouldEqual` "Not Found"

      domainSite = "https://purescript-domain.github.io"
      env = Object.empty # insert "DOMAIN_SITE" domainSite

      fetch = flip (Worker.fetch' sites fetchUpstream) env

    describe "fetch" do
      it "masks root domain site empty path" do
        res <- fetch { url: "http://purescri.pt" }
        res `shouldMask` (domainSite <> "/")
      it "masks root domain site local subpath" do
        res <- fetch { url: "https://purescri.pt/foo.js" }
        res `shouldMask` (domainSite <> "/" <> "foo.js")
      it "redirects configured site empty path to '/' default path" do
        res <- fetch { url: "https://purescri.pt/tecton" }
        res `shouldRedirectTo` "https://purescri.pt/tecton/"
      it
        "redirects configured site subpath without extension to add trailing '/'"
        do
          res <- fetch { url: "http://purescri.pt/tecton/foo" }
          res `shouldRedirectTo` "http://purescri.pt/tecton/foo/"
      it "redirects configured site without masking" do
        res <- fetch { url: "https://purescri.pt/halogen" }
        res `shouldRedirectTo` halogenUrl
      it "redirects configured site without masking subpath" do
        res <- fetch { url: "https://purescri.pt/halogen/issues" }
        res `shouldRedirectTo` (halogenUrl <> "/issues")
      it "masks configured site default '/' path" do
        res <- fetch { url: "http://purescri.pt/tecton/" }
        res `shouldMask` (tectonUrl <> "/")
      it "masks configured site subpath" do
        res <- fetch { url: "https://purescri.pt/tecton/foo/bar.html" }
        res `shouldMask` (tectonUrl <> "/foo/bar.html")
      it "does not serve a disabled site" do
        res <- fetch { url: "https://purescri.pt/chosen" }
        shouldBeDisabled res
      it "does not serve a disabled site's subpaths" do
        res <- fetch { url: "https://purescri.pt/chosen/foo/bar" }
        shouldBeDisabled res
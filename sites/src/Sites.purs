module Domain.Sites where

import Data.Argonaut (Json)
import Foreign.Object (Object)

foreign import sites :: Object Json

foreign import siteNames :: Array String
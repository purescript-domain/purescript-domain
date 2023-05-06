module Cloudflare.Workers.Response where

import Prelude

import Foreign.Object (Object)
import Foreign.Object as FO
import Prim.Row as Row
import Record as Record

foreign import data Response :: Type

type ResponseInit =
  (status :: Int, statusText :: String, headers :: Object String)

mkResponse
  :: forall opts all
   . Row.Nub all ResponseInit
  => Row.Union opts ResponseInit all
  => String
  -> { | opts }
  -> Response
mkResponse body opts = mkResponseImpl body $ Record.merge opts
  ({ status: 200, statusText: "OK", headers: FO.empty } :: { | ResponseInit })

foreign import mkResponseImpl
  :: String
  -> { | ResponseInit }
  -> Response

foreign import status :: Response -> Int

foreign import statusText :: Response -> String

foreign import headers :: Response -> Object String
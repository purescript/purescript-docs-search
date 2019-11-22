module Docs.Search.URIHash
       ( getInput
       , setInput
       , clearInput
       )
where

import Prelude
import Effect (Effect)

foreign import getInput :: Effect String
foreign import setInput :: String -> Effect Unit
foreign import clearInput :: Effect Unit

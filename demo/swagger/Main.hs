{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import Data.Aeson
import Data.ByteString.Lazy.Char8
import Data.Proxy
import Servant
import Servant.Swagger 

type FooBar =
  "foo" :> Capture "x" Bool :> Get '[JSON] String :<|>
  "bar" :> Get '[JSON] Integer

main :: IO ()
main = Data.ByteString.Lazy.Char8.putStrLn . encode $
  toSwagger @(FooBar) Proxy

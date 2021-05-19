{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import Data.Aeson
import Data.ByteString.Lazy.Char8
import Data.Proxy
import Servant
import Servant.Swagger 

type FooBarAPI =
  "foo" :> Capture "x" Bool :> Get '[JSON] String :<|>
  "bar" :> Get '[JSON] Integer :<|>
  "baz" :> ReqBody '[JSON] [String] :> Post '[JSON] [String]

main :: IO ()
main = Data.ByteString.Lazy.Char8.putStrLn . encode $
  toSwagger @(FooBarAPI) Proxy

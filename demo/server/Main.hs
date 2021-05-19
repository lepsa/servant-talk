{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Control.Monad.IO.Class
import Data.Proxy
import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Server

type FooBarAPI =
  "foo" :> Capture "x" Bool :> Get '[JSON] String :<|>
  "bar" :> Get '[JSON] Integer :<|>
  "baz" :> ReqBody '[JSON] [String] :> Post '[JSON] [String]

fooBarAPI :: Proxy FooBarAPI
fooBarAPI = Proxy

foo :: MonadIO m => Bool -> m String
foo = pure . show

bar :: MonadIO m => m Integer
bar = pure 10

baz :: MonadIO m => [String] -> m [String]
baz = pure

fooBarAPIServer :: MonadIO m => ServerT FooBarAPI m
fooBarAPIServer = foo :<|> bar :<|> baz

app :: Application
app = serve fooBarAPI fooBarAPIServer

main :: IO ()
main = Network.Wai.Handler.Warp.run 8080 app

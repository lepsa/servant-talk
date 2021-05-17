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

type FooBar =
  "foo" :> Capture "x" Bool :> Get '[JSON] String :<|>
  "bar" :> Get '[JSON] Integer

fooBar :: Proxy FooBar
fooBar = Proxy

foo :: MonadIO m => Bool -> m String
foo = pure . show

bar :: MonadIO m => m Integer
bar = pure 10

fooBarServer :: MonadIO m => ServerT FooBar m
fooBarServer = foo :<|> bar

app :: Application
app = serve fooBar fooBarServer

main :: IO ()
main = Network.Wai.Handler.Warp.run 8080 app

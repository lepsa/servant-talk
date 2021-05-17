{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Except
import Data.Proxy
import Data.Text
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

type FooBar =
  "foo" :> Capture "x" Bool :> Get '[JSON] String :<|>
  "bar" :> Get '[JSON] Integer

data ApiCalls = ApiCalls
  { foo :: Bool -> ClientM String
  , bar :: ClientM Integer
  }

apiCalls :: ApiCalls
apiCalls = ApiCalls foo bar
  where
    foo :<|> bar = client (Proxy :: Proxy FooBar)

type AppM m a = ReaderT ClientEnv (ExceptT ClientError m) a

getFoo :: MonadIO m => AppM m String
getFoo = do
  env <- ask
  res <- liftIO $ runClientM (foo apiCalls True) env
  either throwError pure res

getBar :: MonadIO m => AppM m Integer 
getBar = do
  env <- ask
  res <- liftIO $ runClientM (bar apiCalls) env
  either throwError pure res

someAppFunction :: MonadIO m => AppM m (String, Integer)
someAppFunction = (,) <$> getFoo <*> getBar

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" 8080 ""
      clientEnv = mkClientEnv manager baseUrl 
  e <- runExceptT $ runReaderT someAppFunction clientEnv
  putStrLn $ show e

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

type FooBarAPI =
  "foo" :> Capture "x" Bool :> Get '[JSON] String :<|>
  "bar" :> Get '[JSON] Integer :<|>
  "baz" :> ReqBody '[JSON] [String] :> Post '[JSON] [String]


data ApiCalls = ApiCalls
  { foo :: Bool -> ClientM String
  , bar :: ClientM Integer
  , baz :: [String] -> ClientM [String]
  }

apiCalls :: ApiCalls
apiCalls = ApiCalls foo bar baz
  where
    foo :<|> bar :<|> baz = client (Proxy :: Proxy FooBarAPI)

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

postBaz :: MonadIO m => [String] -> AppM m [String]
postBaz l = do
  env <- ask
  res <- liftIO $ runClientM (baz apiCalls l) env
  either throwError pure res

someAppFunction :: MonadIO m => AppM m (String, Integer, [String])
someAppFunction = (,,) <$> getFoo <*> getBar <*> postBaz ["a", "b", "c"]

clientEnv :: IO ClientEnv
clientEnv = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" 8080 ""
  pure $ mkClientEnv manager baseUrl 

main :: IO ()
main = do
  env <- clientEnv
  res <- runExceptT $ runReaderT someAppFunction env
  print res
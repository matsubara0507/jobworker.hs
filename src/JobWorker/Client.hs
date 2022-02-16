{-# LANGUAGE RecordWildCards #-}

module JobWorker.Client where

import           Control.Concurrent (threadDelay)
import           Control.Monad      (forever, when)
import qualified Data.Text          as Text
import qualified Network.WebSockets as WS
import           Text.Read          (readMaybe)

data Client = Client
  { host    :: String
  , port    :: Int
  , path    :: String
  , verbose :: Bool
  }

-- dest is expected 'localhost:8080/hoge/fuga'
new :: String -> Bool -> Maybe Client
new dest verbose = do
  port <- readMaybe port'
  pure Client {..}
  where
    (addr, path)  = span (/= '/') dest
    (host, port') = drop 1 <$> span (/= ':') addr

run :: Client -> IO ()
run client = do
  when client.verbose $
    putStrLn ("Connectiong to " ++ toDestination client)
  WS.runClient client.host client.port client.path $ \conn -> do
    when client.verbose $
      putStrLn ("Connected to " ++ toDestination client)
    wid <- Text.unpack <$> WS.receiveData conn
    when client.verbose $
      putStrLn ("Worker ID is " ++ wid)
    _ <- forever (threadDelay 1000000)
    pure ()

toDestination :: Client -> String
toDestination client =
  client.host ++ ":" ++ show client.port ++ client.path

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf            #-}

module Main where

import           Paths_jobworker          (version)

import           Control.Monad            (when)
import qualified Data.Version             as Version
import qualified JobWorker.DB             as JobWorkerDB
import qualified JobWorker.Server         as JobWorker
import qualified Network.Wai.Handler.Warp as Warp
import           Servant                  (serve)
import           System.Console.GetOpt
import           System.Environment       (getArgs)
import           Text.Read                (readMaybe)

main :: IO ()
main = do
  opts <- compilerOpts usage =<< getArgs
  if
    | opts.help    -> putStrLn usage
    | opts.version -> putStrLn $ Version.showVersion version
    | otherwise    -> runServer opts
  where
    usage = usageInfo "Usage: jobworker [OPTION...]" options

runServer :: Options -> IO ()
runServer opts = do
  db <- JobWorkerDB.new
  when opts.verbose $
    putStrLn ("Listening on port " ++ show opts.port)
  Warp.run opts.port $
    serve JobWorker.api (JobWorker.server config db)
  where
    config = JobWorker.Config opts.verbose

data Options = Options
  { help    :: Bool
  , version :: Bool
  , verbose :: Bool
  , port    :: Int
  }

defaultOptions :: Options
defaultOptions = Options
  { help    = False
  , version = False
  , verbose = False
  , port    = 8080
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h'] ["help"]
      (NoArg (\opts -> opts { help = True }))
      "Show this help text"
  , Option ['V'] ["version"]
      (NoArg (\opts -> opts { version = True }))
      "Show version"
  , Option ['v'] ["verbose"]
      (NoArg (\opts -> opts { verbose = True }))
      "Enable verbose mode"
  , Option ['p'] ["port"]
      (ReqArg (\port opts -> maybe opts (\p -> opts { port = p }) (readMaybe port)) "PORT")
      "Port for server (default is 8080)"
  ]

compilerOpts :: String -> [String] -> IO Options
compilerOpts usage argv =
  case getOpt Permute options argv of
    (o, _, []  ) -> pure $ foldl (flip id) defaultOptions o
    (_, _, errs) -> ioError $ userError (concat errs ++ usage)

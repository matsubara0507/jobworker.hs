{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf            #-}

module Main where

import           Paths_jobworker       (version)

import           Data.Maybe            (listToMaybe)
import qualified Data.Version          as Version
import qualified JobWorker.Client      as JobWorkerClient
import           System.Console.GetOpt
import           System.Environment    (getArgs)

main :: IO ()
main = do
  (opts, rest) <- compilerOpts usage =<< getArgs
  if
    | opts.help    -> putStrLn usage
    | opts.version -> putStrLn $ Version.showVersion version
    | otherwise    -> runClient opts rest
  where
    usage = usageInfo "Usage: jobworker-runner [OPTION...] DESTINATION" options

runClient :: Options -> Maybe String -> IO ()
runClient _ Nothing =
  ioError $ userError "please input DESTINATION"
runClient opts (Just dest) = do
  c <- JobWorkerClient.new dest opts.verbose
  case c of
    Just client -> JobWorkerClient.run client
    Nothing     -> ioError $ userError "parse DESTINATION error"

data Options = Options
  { help    :: Bool
  , version :: Bool
  , verbose :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { help    = False
  , version = False
  , verbose = False
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
  ]

compilerOpts :: String -> [String] -> IO (Options, Maybe String)
compilerOpts usage argv =
  case getOpt Permute options argv of
    (o, rest, []) -> pure (foldl (flip id) defaultOptions o, listToMaybe rest)
    (_, _,  errs) -> ioError $ userError (concat errs ++ usage)

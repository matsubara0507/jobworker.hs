module JobWorker.Docker where

import qualified JobWorker.Job  as Job
import           System.Exit    (ExitCode)
import qualified System.Process as Process

run :: Job.Config -> IO (ExitCode, String, String)
run config =
  case config.command of
    Just cmd ->
      Process.readProcessWithExitCode "docker" ["run", "--rm", config.image, cmd] ""
    Nothing ->
      Process.readProcessWithExitCode "docker" ["run", "--rm", config.image] ""

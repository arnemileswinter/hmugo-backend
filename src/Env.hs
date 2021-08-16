module Env where

import ClassyPrelude
import qualified Data.Text as T
import System.Environment (getEnvironment)
import System.Exit (exitFailure)

type ApiKey = String

data Env = Env { envApiKey :: ApiKey }

getEnvIO :: IO Env
getEnvIO = do
  env <- (\envs v -> maybe (Left $ "Environment variable not set: " ++ v) Right $ lookup (T.unpack v) envs) <$> getEnvironment
  either (\err -> putStrLn err >> exitFailure) pure $
    Env <$> env "API_KEY"

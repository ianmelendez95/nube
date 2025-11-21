module Nube.Cli where

import Nube (compileFile)
import Options.Applicative
  ( Parser,
    execParser,
    info,
    long,
    metavar,
    short,
    strArgument,
    strOption,
  )

data Command
  = Build String
  | Invoke String String
  deriving (Show)

run :: IO ()
run = do
  cmd <- parseArgs
  case cmd of
    (Build js_file) -> do
      putStrLn $ "compiling: " <> js_file
      compileFile js_file
      putStrLn "done"
    (Invoke api_id args) -> putStrLn $ "Invoking: " ++ show cmd

parseArgs :: IO Command
parseArgs = execParser (info invokeParser mempty)

invokeParser :: Parser Command
invokeParser =
  Invoke
    <$> strOption (long "api-id" <> short 'a')
    <*> strArgument (metavar "fn_args")
module Nube.Cli where

import Nube (compileFile)
import Options.Applicative
  ( Parser,
    command,
    execParser,
    info,
    long,
    metavar,
    short,
    strArgument,
    strOption,
    subparser,
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
parseArgs = execParser (info cmdParser mempty)

cmdParser :: Parser Command
cmdParser =
  subparser
    ( command "build" (info buildParser mempty)
        <> command "invoke" (info invokeParser mempty)
    )

buildParser :: Parser Command
buildParser =
  Build <$> strArgument (metavar "js_file")

invokeParser :: Parser Command
invokeParser =
  Invoke
    <$> strOption (long "api-id" <> short 'a')
    <*> strArgument (metavar "fn_args")
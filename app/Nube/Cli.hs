module Nube.Cli where

import Nube (compileFile)
import Options.Applicative
  ( Parser,
    command,
    execParser,
    helper,
    hsubparser,
    info,
    long,
    metavar,
    progDesc,
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
parseArgs = execParser (info (helper <*> cmdParser) (progDesc "The Nube all-in-one tool"))

cmdParser :: Parser Command
cmdParser =
  hsubparser
    ( command "build" (info buildParser (progDesc "Build CF resources from a JS file"))
        <> command "invoke" (info invokeParser (progDesc "Invoke a function API"))
    )

buildParser :: Parser Command
buildParser =
  Build <$> strArgument (metavar "js_file")

invokeParser :: Parser Command
invokeParser =
  Invoke
    <$> strOption (long "api-id" <> short 'a')
    <*> strArgument (metavar "fn_args")
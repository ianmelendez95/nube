module Main (main) where

import Nube
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "usage: nube <js-file>"
    [js_file] -> do
      putStrLn $ "compiling: " <> js_file
      compileFile js_file
      putStrLn "done"
    _ -> putStrLn "usage: nube <js-file>"

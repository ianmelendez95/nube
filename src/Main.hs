module Main (main) where

import System.Environment
import Compile

main :: IO ()
main = do 
  args <- getArgs
  case args of 
    [] -> putStrLn "usage: nube <js-file>"
    [js_file] -> do 
      putStrLn $ "compiling: " <> js_file
      compileFile js_file
      putStrLn $ "done"
    _ -> putStrLn "usage: nube <js-file>"

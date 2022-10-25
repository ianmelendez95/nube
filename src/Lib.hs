module Lib where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Lambda.Gen as G
import qualified JS.Parse as P

js_file :: FilePath
js_file = "example/capitalizeWord/src.js"

lambda_file :: FilePath
lambda_file = "example/capitalizeWord/index.js"

main :: IO ()
main = do 
  js <- head <$> P.parseJsFile js_file
  TIO.writeFile lambda_file $ G.jsFunToHandlerScript js

module Lib where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Lambda.Gen as G
import qualified JS.Parse as P

js_file :: FilePath
js_file = "example/capitalizeWords/src.js"

lambda_file :: FilePath
lambda_file = "example/capitalizeWords/index.js"

main :: IO ()
main = do 
  js <- P.parseJsFile js_file
  TIO.writeFile lambda_file $ G.jsFunsToScript (head js) (tail js)

module Lib where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Lambda.Gen as G
import qualified JS.Parse as P

import System.FilePath

main :: IO ()
main = compileFile "example/capitalizeWords/src.js"

compileFile :: FilePath -> IO ()
compileFile src = do
  js <- P.parseJsFile src
  let scripts = G.jsFunsToScripts js
      deploy_script = G.jsFunsToDeployScript js
  G.writeScripts (takeDirectory src </> "dist") deploy_script scripts

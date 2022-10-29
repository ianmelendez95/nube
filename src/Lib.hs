module Lib where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Lambda.Gen as G
import qualified JS.Parse as P

import System.FilePath

js_paths :: [(FilePath, FilePath)]
js_paths = 
  [ ( "example/capitalizeWords/src.js"
    , "example/capitalizeWords/capitalizeWords.js" 
    )
  , ( "example/capitalizeWord/src.js"
    , "example/capitalizeWords/capitalizeWord.js" 
    )
  ]

main :: IO ()
main = mapM_ (compileFile . fst) js_paths

compileFile :: FilePath -> IO ()
compileFile src = do
  js <- P.parseJsFile src
  let scripts = G.jsFunsToScripts js
  G.writeScripts (takeDirectory src </> "dist") scripts

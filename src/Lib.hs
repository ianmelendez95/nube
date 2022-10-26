module Lib where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Lambda.Gen as G
import qualified JS.Parse as P

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
main = mapM_ (uncurry compileFile) js_paths

compileFile :: FilePath -> FilePath -> IO ()
compileFile src dest = do
  js <- P.parseJsFile src
  TIO.writeFile dest $ G.jsFunsToScript (head js) (tail js)

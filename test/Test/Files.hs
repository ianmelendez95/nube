module Test.Files (readTestFile) where 

import Prelude hiding (readFile)

import Data.Text
import Data.Text.IO
import System.FilePath
import System.Directory

readTestFile :: FilePath -> IO Text
readTestFile rel_path =
  if isAbsolute rel_path 
    then readFile rel_path
    else readFile $ "test-files" </> rel_path

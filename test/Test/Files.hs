module Test.Files (readTestFile, parseTestFile) where 

import Prelude hiding (readFile)

import Data.Text
import Data.Text.IO
import System.FilePath
import System.Directory
import Nube.Parser (Parser)
import Test.Util.Parse (testParser)

readTestFile :: FilePath -> IO Text
readTestFile rel_path =
  if isAbsolute rel_path 
    then readFile rel_path
    else readFile $ "test-files" </> rel_path

parseTestFile :: FilePath -> Parser a -> IO a
parseTestFile rel_path p = do 
  content <- readTestFile rel_path
  testParser p content
  

module Nube (compileFile) where

import Control.Monad
  ( unless,
    (>=>),
  )
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Char
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Debug.Trace (trace, traceShowId)
import Gen.CF qualified as CF
import Gen.Lambda qualified as GL
import Nube.Compiler (Compiler, CompilerT, runCompiler, runCompilerT)
import Nube.Cont (ContSplit, splitContInScript)
import Nube.Context (NContext (NContext))
import Nube.St (compileScriptSt)
import Nube.Parse qualified as P
import Nube.Rename (renameInScript)
import Nube.Ret (compileReturns)
import Nube.Syntax qualified as S
import System.FilePath
  ( takeBaseName,
    takeDirectory,
    takeExtension,
    (</>),
  )
import Util.Aeson (writeFileJSON)

-- test_js_file = "example/capitalizeWords/capitalizeWords.js"

compileFile :: FilePath -> IO ()
compileFile js_file = do
  assertValidJsFileName js_file

  js_text <- TIO.readFile js_file
  (js_script, ctx) <- P.parseJsContent js_file js_text
  script@(S.Script script_name script_fns) <- compileScript ctx js_script
  let scripts = GL.jsFunsToScripts script_fns
      deploy_script = GL.jsScriptToDeployScript script
  GL.writeScripts
    (takeDirectory js_file </> "dist")
    deploy_script
    scripts

  let template = CF.templateFromScript script
  writeFileJSON (templateFilePath script_name) template
  where
    templateFilePath script_name =
      dist_dir </> T.unpack (GL.templateNameFromScriptName script_name)

    dist_dir = takeDirectory js_file </> "dist"

-- compileScript :: S.Script -> Compiler S.Script
-- compileScript (S.Script s_name s_fns) = undefined

assertValidJsFileName :: FilePath -> IO ()
assertValidJsFileName file_path =
  let ext = takeExtension file_path
      basename = takeBaseName file_path
   in if ext /= ".js"
        then fail $ "Expecting .js file extension - has: " <> ext
        else
          unless
            (all isLetter basename)
            (fail $ "Only letters allowed in file basename: " <> basename)

compileScript :: NContext -> S.Script -> IO S.Script
compileScript ctx = either fail pure . runCompiler ctx . compileScriptC

compileScriptC :: S.Script -> Compiler S.Script
compileScriptC = compileScriptSt >=> renameInScript >=> compileReturns

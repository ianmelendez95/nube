module Nube where

import Control.Monad
  ( unless,
  )
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Char
import Data.Text qualified as T
import Gen.CF qualified as CF
import Gen.Lambda qualified as GL
import Nube.Compiler (Compiler)
import Nube.Cont (ContSplit, splitStmtContinuations)
import Nube.Context (NContext (NContext))
import Nube.Parse qualified as P
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

  (script, ctx) <- P.parseJsFile js_file
  let js = S.scriptFns script
      proxies_script = GL.jsFunsToProxiesScript js
      scripts = GL.jsFunsToScripts js
      deploy_script = GL.jsScriptToDeployScript script
  GL.writeScripts
    (takeDirectory js_file </> "dist")
    deploy_script
    proxies_script
    scripts

  let template = CF.templateFromScript script
  writeFileJSON (templateFilePath $ S.scriptName script) template
  where
    templateFilePath script_name =
      dist_dir </> T.unpack (GL.templateNameFromScriptName script_name)

    dist_dir = takeDirectory js_file </> "dist"

assertValidJsFileName :: FilePath -> IO ()
assertValidJsFileName file_path =
  let ext = takeExtension file_path
      basename = takeBaseName file_path
   in if ext /= ".js"
        then error $ "Expecting .js file extension - has: " <> ext
        else
          unless
            (all isLetter basename)
            (error $ "Only letters allowed in file basename: " <> basename)

compileScript :: S.Script -> Either String S.Script
compileScript (S.Script name fns) =
  let ctx = NContext (map S.fnName fns)
   in runCompiler ctx $ do
        fns' <- mconcat <$> mapM compileFunction fns
        pure $ S.Script name fns'

-- compileStatement :: NContext -> S.Stmt -> Either String S.Stmt
-- compileStatement ctx = runCompiler ctx . tStatement

compileFunction :: S.Fn -> Compiler [S.Fn]
compileFunction (S.Fn _name _params stmts) = do
  splits <- splitStmtContinuations _name stmts
  undefined

runCompiler :: NContext -> Compiler a -> Either String a
runCompiler context transpiler = runExcept $ runReaderT transpiler context
{- | UPLC textual pretty-printer used by treefmt as the @*.uplc@ formatter.

Reads each path passed on argv, parses it as a UPLC program, and rewrites
the file in place with the canonical 'prettyPlcClassic' rendering. The
output format matches what 'Cape.WritePlc.writeCodeToFile' produces, so
running this tool over a freshly generated submission is a no-op.
-}
module Main (main) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text.IO as T
import qualified PlutusCore as PLC
import qualified PlutusCore.Pretty as PP
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import qualified UntypedPlutusCore as UPLC
import qualified UntypedPlutusCore.Parser as UPLCParser

main :: IO ()
main = do
  paths <- getArgs
  results <- traverse formatFile paths
  if and results then exitSuccess else exitFailure

formatFile :: FilePath -> IO Bool
formatFile path = do
  src <- T.readFile path
  case PLC.runQuote (runExceptT (UPLCParser.parseProgram src)) ::
    Either
      (PLC.Error PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan)
      (UPLC.Program UPLC.Name PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan) of
    Left err -> do
      hPutStrLn stderr (path <> ": " <> show err)
      pure False
    Right prog -> do
      writeFile path (show (PP.prettyPlcClassic prog))
      pure True

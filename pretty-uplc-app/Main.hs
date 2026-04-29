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
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (Handle, hPutStrLn, stderr, stdout)
import qualified UntypedPlutusCore as UPLC
import qualified UntypedPlutusCore.Parser as UPLCParser

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage stdout >> exitSuccess
    a : _ | a `elem` ["-h", "--help"] -> usage stdout >> exitSuccess
    _ -> case filter ("-" `isPrefix`) args of
      bad@(_ : _) -> do
        progName <- getProgName
        hPutStrLn stderr $ progName <> ": unknown option(s): " <> unwords bad
        usage stderr
        exitFailure
      [] -> do
        results <- traverse formatFile args
        if and results then exitSuccess else exitFailure
  where
    isPrefix p s = take (length p) s == p

usage :: Handle -> IO ()
usage h = do
  progName <- getProgName
  hPutStrLn h $
    unlines
      [ "Usage: " <> progName <> " FILE..."
      , ""
      , "Pretty-print one or more UPLC files in place using the canonical"
      , "PlutusCore.Pretty.prettyPlcClassic renderer (the same format produced"
      , "by Cape.WritePlc, so the tool is idempotent on freshly generated"
      , "Plinth submissions)."
      , ""
      , "Options:"
      , "  -h, --help    Show this help message and exit."
      , ""
      , "Exit status is non-zero if any file fails to parse; the parse error is"
      , "written to stderr in the form '<path>: <error>'."
      ]

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

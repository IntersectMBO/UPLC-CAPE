{- | UPLC textual pretty-printer used by treefmt as the @*.uplc@ formatter.

Reads each path passed on argv, parses it as a UPLC program, and rewrites
the file in place with the canonical 'prettyPlcClassic' rendering. The
output format matches what 'Cape.WritePlc.writeCodeToFile' produces, so
running this tool over a freshly generated submission is a no-op.
-}
module Main (main) where

import Prelude

import Data.List qualified as L
import Data.Text.IO qualified as Text.IO
import Options.Applicative qualified as Opts
import PlutusCore qualified as PLC
import PlutusCore.Pretty qualified as PP
import System.IO (hPutStrLn)
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Parser qualified as UPLCParser

newtype Cli = Cli {cliFiles :: [FilePath]}

cliParser :: Opts.Parser Cli
cliParser =
  Cli
    <$> Opts.some
      ( Opts.strArgument $
          Opts.metavar "FILE..."
            <> Opts.help "UPLC file to pretty-print in place"
      )

cliInfo :: Opts.ParserInfo Cli
cliInfo =
  Opts.info (cliParser Opts.<**> Opts.helper) $
    Opts.fullDesc
      <> Opts.header "pretty-uplc — canonical UPLC textual formatter"
      <> Opts.progDesc
        ( "Pretty-print one or more UPLC files in place using "
            <> "PlutusCore.Pretty.prettyPlcClassic. Idempotent on freshly "
            <> "generated Plinth submissions because Cape.WritePlc uses the "
            <> "same printer."
        )
      <> Opts.footer
        ( "Exit status is non-zero if any file fails to parse; the parse "
            <> "error is written to stderr in the form '<path>: <error>'."
        )

main :: IO ()
main = do
  Cli {cliFiles} <- Opts.execParser cliInfo
  results <- traverse formatFile cliFiles
  if and results then exitSuccess else exitFailure

formatFile :: FilePath -> IO Bool
formatFile path = do
  src <- Text.IO.readFile path
  case PLC.runQuote (runExceptT (UPLCParser.parseProgram src)) ::
        Either
          (PLC.Error PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan)
          (UPLC.Program UPLC.Name PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan) of
    Left err -> do
      hPutStrLn stderr (path <> ": " <> show err)
      pure False
    Right prog -> do
      -- Show on prettyPlcClassic produces no trailing newline; normalise to
      -- a single trailing '\n' so GitHub stops flagging "no newline at end
      -- of file" and the formatter is idempotent across editors that
      -- auto-add one.
      let rendered = show (PP.prettyPlcClassic prog)
          normalised = L.dropWhileEnd (== '\n') rendered <> "\n"
      writeFile path normalised
      pure True

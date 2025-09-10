module Cape.Cli (
  Options (..),
  parseOptions,
) where

import Options.Applicative
import Prelude

{- | CLI options
-i: input UPLC file
-o: output metrics.json file
-t: required cape-tests.json file for test execution
-}
data Options = Options
  { optInput :: FilePath
  , optOutput :: FilePath
  , optTests :: FilePath
  , optValidateOnly :: Bool
  , optDebugContext :: Bool
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption
      ( long "input"
          <> short 'i'
          <> metavar "FILE"
          <> help "UPLC input file"
      )
    <*> strOption
      ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> help "metrics.json output file"
      )
    <*> strOption
      ( long "tests"
          <> short 't'
          <> metavar "TESTS.json"
          <> help
            "Test specification file (cape-tests.json) for test execution"
      )
    <*> switch
      ( long "validate-only"
          <> help "Run validation tests only, do not generate metrics.json"
      )
    <*> switch
      ( long "debug-context"
          <> help "Output BuiltinData encoding for ScriptContext inputs (for debugging)"
      )

optsInfo :: ParserInfo Options
optsInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc
          "Measure a UPLC program and run comprehensive tests using cape-tests.json"
        <> header "uplc-measure"
    )

-- | Parse command-line options
parseOptions :: IO Options
parseOptions = execParser optsInfo

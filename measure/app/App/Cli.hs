module App.Cli (
  Options (..),
  parseOptions,
) where

import Options.Applicative
import Prelude

-- | CLI options
-- -i: input UPLC file
-- -o: output metrics.json file
-- -t: optional cape-tests.json file for unified test execution
data Options = Options
  { optInput :: FilePath
  , optOutput :: FilePath
  , optTests :: Maybe FilePath
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
    <*> optional
      ( strOption
          ( long "tests"
              <> short 't'
              <> metavar "TESTS.json"
              <> help
                "Test specification file (cape-tests.json) for unified test execution"
          )
      )

optsInfo :: ParserInfo Options
optsInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc
          "Measure a UPLC program and optionally run comprehensive tests using cape-tests.json"
        <> header "uplc-measure"
    )

-- | Parse command-line options
parseOptions :: IO Options
parseOptions = execParser optsInfo

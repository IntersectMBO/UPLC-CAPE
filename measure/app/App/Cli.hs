module App.Cli (
  Options (..),
  parseOptions,
) where

import Options.Applicative
import Prelude

-- | CLI options
-- -i: input UPLC file
-- -o: output metrics.json file
-- -v: optional verifier UPLC file (applied as (verifier submissionResult))
-- -d: optional driver UPLC file for validator testing with overhead calculation
data Options = Options
  { optInput :: FilePath
  , optOutput :: FilePath
  , optVerifier :: Maybe FilePath
  , optDriver :: Maybe FilePath
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption
      (long "input" <> short 'i' <> metavar "FILE" <> help "UPLC input file")
    <*> strOption
      ( long "output" <> short 'o' <> metavar "FILE" <> help "metrics.json output file"
      )
    <*> optional
      ( strOption
          ( long "verifier"
              <> short 'v'
              <> metavar "VERIFIER.uplc"
              <> help
                "Verifier UPLC file applied as (verifier submissionResult) for correctness verification"
          )
      )
    <*> optional
      ( strOption
          ( long "driver"
              <> short 'd'
              <> metavar "DRIVER.uplc"
              <> help
                "Driver UPLC file for validator testing with overhead calculation"
          )
      )

optsInfo :: ParserInfo Options
optsInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc
          "Measure a UPLC program and optionally verify correctness using a provided verifier UPLC file or test validators using a driver UPLC file"
        <> header "uplc-measure"
    )

-- | Parse command-line options
parseOptions :: IO Options
parseOptions = execParser optsInfo

module Cape.Error (
  MeasureError (..),
  renderMeasureError,
  exitCodeForError,
) where

import Control.Exception qualified as E
import System.Exit (ExitCode (..))
import Prelude

-- | Custom exception types for business logic errors
data MeasureError
  = FileNotFoundError FilePath
  | FileDecodeError FilePath
  | UPLCParseError FilePath String
  | EvaluationError String
  | VerificationError String
  | DriverError String
  deriving stock (Show)

instance E.Exception MeasureError where
  displayException = renderMeasureError

-- | Render MeasureError to user-friendly message
renderMeasureError :: MeasureError -> String
renderMeasureError err =
  case err of
    FileNotFoundError path ->
      "Error: File not found: " <> path
    FileDecodeError path ->
      "Error: Failed to decode UTF-8 text: " <> path
    UPLCParseError path details ->
      "Error: Malformed/invalid UPLC file: " <> path <> "\n" <> details
    EvaluationError details ->
      "Error: Evaluation failed: " <> details
    VerificationError details ->
      "Error: Verification failed: " <> details
    DriverError details ->
      "Error: Driver operation failed: " <> details

-- | Get exit code for MeasureError
exitCodeForError :: MeasureError -> ExitCode
exitCodeForError err =
  case err of
    FileNotFoundError _ -> ExitFailure 2
    FileDecodeError _ -> ExitFailure 4
    UPLCParseError _ _ -> ExitFailure 4
    EvaluationError _ -> ExitFailure 3
    VerificationError _ -> ExitFailure 2
    DriverError _ -> ExitFailure 3

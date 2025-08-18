{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Main where

import PlutusTx qualified as Tx
import PlutusTx.Prelude qualified as P
import UntypedPlutusCore qualified as UPLC
import PlutusCore.Pretty (prettyPlcClassic)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Prelude (IO, FilePath, ($), (++), putStrLn, show)
import Prelude qualified

-- | Verifier that checks if the input integer equals 42
verifyEquals42 :: Prelude.Integer -> ()
verifyEquals42 n = 
  if n P.== 42 
  then () 
  else P.error ()

-- | Compile the verifier to UPLC  
verifyEquals42Compiled :: Tx.CompiledCode (Prelude.Integer -> ())
verifyEquals42Compiled = $$(Tx.compile [|| verifyEquals42 ||])

-- | Generate UPLC text for the verifier
generateVerifierUPLC :: IO Text
generateVerifierUPLC = do
  let compiled = verifyEquals42Compiled
      program = Tx.getPlcNoAnn compiled
      doc = prettyPlcClassic program
  Prelude.return $ Text.pack $ show doc

-- | Write the verifier UPLC to a file
writeVerifierToFile :: FilePath -> IO ()
writeVerifierToFile filepath = do
  uplcText <- generateVerifierUPLC
  Text.writeFile filepath uplcText
  putStrLn $ "Verifier UPLC written to: " ++ filepath

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filepath] -> writeVerifierToFile filepath
    [] -> do
      uplcText <- generateVerifierUPLC
      Text.putStrLn uplcText
    _ -> do
      putStrLn "Usage: generate-verifiers [output-file]"
      putStrLn "  Without output-file: prints UPLC to stdout"
      putStrLn "  With output-file: writes UPLC to file"
      exitFailure
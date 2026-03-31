{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-preserve-logging #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:datatypes=BuiltinCasing #-}

-- | Re-compilation of FibonacciIterative with BuiltinCasing enabled
module Preview.FibonacciIterative (fibonacciIterativeCode) where

import FibonacciIterative (fibonacciIterative)
import PlutusTx
import PlutusTx.Prelude

fibonacciIterativeCode :: CompiledCode (Integer -> Integer)
fibonacciIterativeCode = $$(PlutusTx.compile [||fibonacciIterative||])

{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
--
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-preserve-logging #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module FibonacciIterative (fibonacciIterativeCode) where

import PlutusTx
import PlutusTx.Prelude

-- | Compiled iterative fibonacci function
fibonacciIterativeCode :: CompiledCode (Integer -> Integer)
fibonacciIterativeCode = $$(PlutusTx.compile [||fibonacciIterative||])

{-# INLINEABLE fibonacciIterative #-}
fibonacciIterative :: Integer -> Integer
fibonacciIterative n
  | n <= 1 = n
  | otherwise = fibonacciLoop 0 1 2 n

{-# INLINEABLE fibonacciLoop #-}
fibonacciLoop :: Integer -> Integer -> Integer -> Integer -> Integer
fibonacciLoop prev curr i n
  | i > n = curr
  | otherwise = fibonacciLoop curr (prev + curr) (i + 1) n

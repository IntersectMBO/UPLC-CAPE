module Main (main) where

import Compile (compileToUplc)
import qualified Data.ByteString as BS
import Plutarch.Builtin.ByteString
import Plutarch.Internal.Term (punsafeBuiltin)
import Plutarch.Prelude
import qualified PlutusCore as PLC

fibSeq :: Term s PByteString
fibSeq =
  pconstant $
    BS.pack
      [ 0
      , 0
      , 0
      , 0
      , 0
      , 1
      , 0
      , 0
      , 1
      , 0
      , 0
      , 2
      , 0
      , 0
      , 3
      , 0
      , 0
      , 5
      , 0
      , 0
      , 8
      , 0
      , 0
      , 13
      , 0
      , 0
      , 21
      , 0
      , 0
      , 34
      , 0
      , 0
      , 55
      , 0
      , 0
      , 89
      , 0
      , 0
      , 144
      , 0
      , 0
      , 233
      , 0
      , 1
      , 121
      , 0
      , 2
      , 98
      , 0
      , 3
      , 219
      , 0
      , 6
      , 61
      , 0
      , 10
      , 24
      , 0
      , 16
      , 95
      , 0
      , 26
      , 109
      , 0
      , 42
      , 194
      , 0
      , 69
      , 47
      , 0
      , 111
      , 241
      , 0
      , 181
      , 32
      , 1
      , 37
      , 17
      ]

-- conditional without hoisting the IfThenElse builtin.
pif'' :: Term s PBool -> Term s a -> Term s a -> Term s a
pif'' cond ifT ifF =
  pforce $
    pforce (punsafeBuiltin PLC.IfThenElse) # cond # pdelay ifT # pdelay ifF

pfibo :: Term s (PInteger :--> PInteger)
pfibo = plam $ \x ->
  pif''
    (x #< 0)
    x
    ( ( pbyteStringToInteger
          # pmostSignificantFirst
          #$ psliceBS
          # (x * 3)
          # 3
          # fibSeq
      )
    )

main :: IO ()
main = compileToUplc "fibonacci_prepacked.uplc" pfibo

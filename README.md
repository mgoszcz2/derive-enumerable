# Derive-enumerable

Generalized equivalent to doing `[minBound..maxBound]` but on complex types.


    λ :m +Data.Enumerable.Generic
    λ :set -XDeriveGeneric
    λ data Flag = Flag Bool Word8 deriving (Show, Generic)
    λ instance Enumerable Flag
    λ instance Default Flag where def = Flag False 0
    λ allEnum :: [Flag]
    [Flag False 0,Flag True 0,Flag False 1, (..snip..) Flag False 255,Flag True 255]


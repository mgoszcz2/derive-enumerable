{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Copyright   : Maciej Goszczycki 2015
-- License     : GPL-3
-- Maintainer  : mgoszcz2@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- `Enumerable` provides a generalized equivalent to doing `[minBound..maxBound]` but on complex types.
-- 
-- > ([minBound..maxBound] :: [Word8]) == allEnums


module Data.Enumerable.Generic (
    -- * Example usage
    -- $usage

    -- ** Multi-consturctor types
    -- $constructors

    -- ** Without `Default` instances
    -- $nodefault
    
    -- * Caveats & Extending
    -- $info

    -- * Convenience functions
      allEnum
    , allConstEnum
    , allDefsEnum
    , defEnumerable
    -- * Type-classes
    , Enumerable(..)
    , Defaults(..)
    -- * Extra `Default` instances
    , Default(..)
    ) where


import GHC.Generics
import Data.Default (Default, def)
import Control.Arrow (first, second)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)


-- | Generate all possible variations of a type
allEnum :: (Default x, Enumerable x) => [x]
allEnum = allNext def

-- | Generate all possible variations of a type given a list of default values of constructors
allConstEnum :: (Enumerable x) => [x] -> [x]
allConstEnum = concatMap allNext

-- | Same as `allConstEnum` but automatically get constructors from `Defaults` class
allDefsEnum :: (Defaults x, Enumerable x) => [x]
allDefsEnum = allConstEnum defs


-- | Class used for deriving generic `Enumerable` instances
class GEnumerable x where
    gper :: x a -> (x a, Bool)


-- | Class for easier enumeration of multi-constructor types
class Defaults x where
    defs :: [x]


-- | Class for generating enumerations of arbitrary data types
class Enumerable x where
    -- | Minimum definition of `Enumerable`. Due to haskell's inability to
    -- deal with overlapping instances, only defined by `GEnumerable` derived types
    
    -- | Given a value returns next enumeration and a carry boolean, set to `True`
    -- if returning final enumeration
    per :: x -> (x, Bool)
    default per :: (Generic x, GEnumerable (Rep x)) => x -> (x, Bool)
    per = first to . gper . from
    {-# MINIMAL per #-}

    -- | Symmetric `per`. Only enumerate if the carry boolean is `True`
    sper :: (x, Bool) -> (x, Bool)
    sper (x, False) = (x, False)
    sper (x, True) = per x

    -- | Next enumeration. Call `per` discarding the carry boolean
    next :: x -> x
    next = fst . per

    -- | Return all enumerations of a starting value
    allNext :: x -> [x]
    allNext x = map fst . takeWhile (not . snd) $ iterate (sper . second (const True)) (x, False)


-- | Default implementation of `per` used by all simple `Enumerable` instances
-- that are also instances of `Eq`, `Enum` and `Bounded`
defEnumerable :: (Eq x, Enum x, Bounded x) => x -> (x, Bool)
defEnumerable x = if maxBound == x then (minBound, True) else (succ x, False)


-- | Missing `Bool` `Default` instance
instance Default Bool where def = False
-- | Default `Char` is NULL
instance Default Char where def = '\0'
-- | Default `Either` is Left (a false value just like Default Bool)
instance (Default a, Default b) => Default (Either a b) where
    def = Left def

instance Enumerable Bool where per = defEnumerable
instance Enumerable Char where per = defEnumerable
instance Enumerable Int where per = defEnumerable
instance Enumerable Int8 where per = defEnumerable
instance Enumerable Int16 where per = defEnumerable
instance Enumerable Int32 where per = defEnumerable
instance Enumerable Int64 where per = defEnumerable
instance Enumerable Ordering where per = defEnumerable
instance Enumerable Word where per = defEnumerable
instance Enumerable Word8 where per = defEnumerable
instance Enumerable Word16 where per = defEnumerable
instance Enumerable Word32 where per = defEnumerable
instance Enumerable Word64 where per = defEnumerable
instance Enumerable () where per = defEnumerable

instance (Default a, Default b, Enumerable a, Enumerable b) => Enumerable (Either a b) where
    per (Left a) = (if carry then Right def else Left nexta, False)
        where (nexta, carry) = per a
    per (Right b) = (if carry then Left def else Right nextb, carry)
        where (nextb, carry) = per b

instance (Default x, Enumerable x) => Enumerable (Maybe x) where
    per Nothing = (Just def, False)
    per (Just a) = (if carry then Nothing else Just nexta, carry)
        where (nexta, carry) = per a

-- | Enumerates left followed by right
instance (Enumerable a, Enumerable b) => Enumerable (a, b) where
    per (a, b) = if carrya then ((a', b'), carryb) else ((a', b), False)
        where (a', carrya) = per a
              (b', carryb) = per b


instance (GEnumerable x, GEnumerable y) => GEnumerable (x :*: y) where
    gper (x :*: y) = if carry then first (ntype :*:) $ gper y
                              else (ntype :*: y, False)
        where (ntype, carry) = gper x

instance (GEnumerable x, GEnumerable y) => GEnumerable (x :+: y) where
    gper (L1 x) = first L1 $ gper x
    gper (R1 x) = first R1 $ gper x

instance (GEnumerable x) => GEnumerable (M1 i c x) where
    gper (M1 x) = first M1 $ gper x

instance (Enumerable x) => GEnumerable (K1 i x) where
    gper (K1 x) = first K1 $ per x

instance GEnumerable (U1) where
    gper U1 = (U1, True)


-- $info
-- `Defaults` class is just a way of providing default arguments to `allConstEnum`
-- 
-- Elegantly handling multi-constructor types with type-variables and implementing
-- the corresponding `Enumerable` instance is left to the user. Implementations for `Maybe` and `Either`
-- and provided out of the box


-- $usage
-- > λ :set -XDeriveGeneric
-- > λ data Flag = Flag Bool Word8 deriving (Show, Generic)
-- > λ instance Enumerable Flag
-- > λ instance Default Flag where def = Flag False 0
-- > λ allEnum :: [Flag]
-- > [Flag False 0,Flag True 0,Flag False 1,Flag True 1, (..snip..) Flag False 255,Flag True 255]


-- $constructors
-- > λ data Value = A Bool | B | C deriving (Show, Generic)
-- > λ instance Enumerable Value
-- > λ allConstEnum [A False, B, C]
-- > [A False,A True,B,C]
-- > λ instance Defaults Value where defs = [A False, B, C]
-- > λ allDefsEnum :: [Value]
-- > [A False,A True,B,C]


-- $nodefault
-- > λ next False
-- > True
-- > λ next $ 8086 :: Int
-- > 8087
-- > λ next $ Flag True 42
-- > Flag False 43

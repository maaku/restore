-- Copyright (c) 2006-2019 The Binary Developers.
-- Copyright (c) 2019 The Restore Developers.
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0.  If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, KindSignatures,
    ScopedTypeVariables, TypeOperators, TypeSynonymInstances #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#if __GLASGOW_HASKELL__ >= 800
#define HAS_DATA_KIND
#endif

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Restore.Generic
-- Copyright   : Bryan O'Sullivan, Mark Friedenbach
-- License     : Mozilla Public License, v. 2.0 (see: LICENSE)
--
-- Maintainer  : Mark Friedenbach <mark@friedenbach.org>
-- Stability   : unstable
-- Portability : Only works with GHC 7.2 and newer
--
-- Instances for supporting GHC generics.
--
-----------------------------------------------------------------------------
module Data.Restore.Generic
    (
    ) where

import Control.Applicative
import Data.Restore.Class
import Data.Restore.Get
import Data.Restore.Put
import Data.Bits
import Data.Word
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
#ifdef HAS_DATA_KIND
import Data.Kind
#endif
import GHC.Generics
import Prelude -- Silence AMP warning.

-- Type without constructors
instance GRestorePut V1 where
    gput _ = pure ()

instance GRestoreGet V1 where
    gget   = return undefined

-- Constructor without arguments
instance GRestorePut U1 where
    gput U1 = pure ()

instance GRestoreGet U1 where
    gget    = return U1

-- Product: constructor with parameters
instance (GRestorePut a, GRestorePut b) => GRestorePut (a :*: b) where
    gput (x :*: y) = gput x <> gput y

instance (GRestoreGet a, GRestoreGet b) => GRestoreGet (a :*: b) where
    gget = (:*:) <$> gget <*> gget

-- Metadata (constructor name, etc)
instance GRestorePut a => GRestorePut (M1 i c a) where
    gput = gput . unM1

instance GRestoreGet a => GRestoreGet (M1 i c a) where
    gget = M1 <$> gget

-- Constants, additional parameters, and rank-1 recursion
instance Restore a => GRestorePut (K1 i a) where
    gput = put . unK1

instance Restore a => GRestoreGet (K1 i a) where
    gget = K1 <$> get

-- Borrowed from the cereal package.

-- The following GRestore instance for sums has support for serializing
-- types with up to 2^64-1 constructors. It will use the minimal
-- number of bytes needed to encode the constructor. For example when
-- a type has 2^8 constructors or less it will use a single byte to
-- encode the constructor. If it has 2^16 constructors or less it will
-- use two bytes, and so on till 2^64-1.

#define GUARD(WORD) (size - 1) <= fromIntegral (maxBound :: WORD)
#define PUTSUM(WORD) GUARD(WORD) = putSum (0 :: WORD) (fromIntegral size)
#define GETSUM(WORD) GUARD(WORD) = (get :: Get WORD) >>= checkGetSum (fromIntegral size)

instance ( GSumPut  a, GSumPut  b
         , SumSize    a, SumSize    b) => GRestorePut (a :+: b) where
    gput | PUTSUM(Word8) | PUTSUM(Word16) | PUTSUM(Word32) | PUTSUM(Word64)
         | otherwise = sizeError "encode" size
      where
        size = unTagged (sumSize :: Tagged (a :+: b) Word64)

instance ( GSumGet  a, GSumGet  b
         , SumSize    a, SumSize    b) => GRestoreGet (a :+: b) where
    gget | GETSUM(Word8) | GETSUM(Word16) | GETSUM(Word32) | GETSUM(Word64)
         | otherwise = sizeError "decode" size
      where
        size = unTagged (sumSize :: Tagged (a :+: b) Word64)

sizeError :: Show size => String -> size -> error
sizeError s size =
    error $ "Can't " ++ s ++ " a type with " ++ show size ++ " constructors"

------------------------------------------------------------------------

checkGetSum :: (Ord word, Num word, Bits word, GSumGet f)
            => word -> word -> Get (f a)
checkGetSum size code | code < size = getSum code size
                      | otherwise   = fail "Unknown encoding for constructor"
{-# INLINE checkGetSum #-}

class GSumGet f where
    getSum :: (Ord word, Num word, Bits word) => word -> word -> Get (f a)

class GSumPut f where
    putSum :: (Num w, Bits w, Restore w) => w -> w -> f a -> Put

instance (GSumGet a, GSumGet b) => GSumGet (a :+: b) where
    getSum !code !size | code < sizeL = L1 <$> getSum code           sizeL
                       | otherwise    = R1 <$> getSum (code - sizeL) sizeR
        where
          sizeL = size `shiftR` 1
          sizeR = size - sizeL

instance (GSumPut a, GSumPut b) => GSumPut (a :+: b) where
    putSum !code !size s = case s of
                             L1 x -> putSum code           sizeL x
                             R1 x -> putSum (code + sizeL) sizeR x
        where
          sizeL = size `shiftR` 1
          sizeR = size - sizeL

instance GRestoreGet a => GSumGet (C1 c a) where
    getSum _ _ = gget

instance GRestorePut a => GSumPut (C1 c a) where
    putSum !code _ x = put code <> gput x

------------------------------------------------------------------------

class SumSize f where
    sumSize :: Tagged f Word64

#ifdef HAS_DATA_KIND
newtype Tagged (s :: Type -> Type) b = Tagged {unTagged :: b}
#else
newtype Tagged (s :: * -> *)       b = Tagged {unTagged :: b}
#endif

instance (SumSize a, SumSize b) => SumSize (a :+: b) where
    sumSize = Tagged $ unTagged (sumSize :: Tagged a Word64) +
                       unTagged (sumSize :: Tagged b Word64)

instance SumSize (C1 c a) where
    sumSize = Tagged 1

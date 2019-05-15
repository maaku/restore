-- Copyright (c) 2006-2019 The Binary Developers.
-- Copyright (c) 2019 The Restore Developers.
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0.  If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arbitrary where

import Test.QuickCheck

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
#if MIN_VERSION_bytestring(0,10,4)
import qualified Data.ByteString.Short as S
#endif

instance Arbitrary L.ByteString where
  arbitrary = fmap L.fromChunks arbitrary

instance Arbitrary B.ByteString where
  arbitrary = B.pack `fmap` arbitrary

#if MIN_VERSION_bytestring(0,10,4)
instance Arbitrary S.ShortByteString where
  arbitrary = S.toShort `fmap` arbitrary
#endif

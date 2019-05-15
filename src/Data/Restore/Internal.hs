-- Copyright (c) 2006-2019 The Binary Developers.
-- Copyright (c) 2019 The Restore Developers.
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0.  If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{-# LANGUAGE CPP #-}

module Data.Restore.Internal 
 ( accursedUnutterablePerformIO ) where

#if MIN_VERSION_bytestring(0,10,6)
import Data.ByteString.Internal( accursedUnutterablePerformIO )
#else
import Data.ByteString.Internal( inlinePerformIO )

{-# INLINE accursedUnutterablePerformIO #-}
-- | You must be truly desperate to come to me for help.
accursedUnutterablePerformIO :: IO a -> a
accursedUnutterablePerformIO = inlinePerformIO
#endif

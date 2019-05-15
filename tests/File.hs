-- Copyright (c) 2006-2019 The Binary Developers.
-- Copyright (c) 2019 The Restore Developers.
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0.  If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{-# LANGUAGE CPP #-}
module Main where

#if ! MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

import           System.Directory          (getTemporaryDirectory)
import           System.FilePath           ((</>))
import           Test.HUnit

import           Distribution.Simple.Utils (withTempDirectory)
import           Distribution.Verbosity    (silent)

import           Data.Restore

data Foo = Bar !Word32 !Word32 !Word32 deriving (Eq, Show)

instance Restore Foo where
  get = Bar <$> get <*> get <*> get
  put (Bar a b c) = put (a,b,c)

exampleData :: [Foo]
exampleData = make bytes
  where
    make (a:b:c:xs) = Bar a b c : make xs
    make _ = []
    bytes = take (256*1024) (cycle [minBound..maxBound])

readWriteTest :: Test
readWriteTest = TestCase $ do
  tmpDir <- getTemporaryDirectory
  withTempDirectory silent tmpDir "foo-dir" $ \dir -> do
    let fn = dir </> "foo.bin"
    encodeFile fn exampleData
    content <- decodeFile fn
    -- It'd be nice to use lsof to verify that 'fn' isn't still open.
    exampleData @=? content

main :: IO ()
main = do 
  _ <- runTestTT readWriteTest
  return ()
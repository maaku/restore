{-# LANGUAGE DeriveGeneric, StandaloneDeriving, BangPatterns #-}
module Main where

import qualified Data.ByteString.Lazy            as L
import           Cabal24 (PackageDescription)

import           Criterion.Main

import qualified Data.Restore                     as Restore
import           Data.Restore.Get                 (Get)
import qualified Data.Restore.Get                 as Restore

import           GenericsBenchCache

main :: IO ()
main = benchmark =<< readPackageDescriptionCache 100

benchmark :: [PackageDescription] -> IO ()
benchmark pds = do
  let lbs = encode pds
      !_ = L.length lbs
      str = show pds
      !_ = length str
  defaultMain [
      bench "encode" (nf encode pds)
    , bench "decode" (nf decode lbs)
    , bench "decode null" (nf decodeNull lbs)
    , bgroup "embarrassment" [
          bench "read" (nf readPackageDescription str)
        , bench "show" (nf show pds)
      ]
    ]

encode :: [PackageDescription] -> L.ByteString
encode = Restore.encode

decode :: L.ByteString -> Int
decode = length . (Restore.decode :: L.ByteString -> [PackageDescription])

decodeNull :: L.ByteString -> ()
decodeNull =
  Restore.runGet $ do
    n <- Restore.get :: Get Int
    go n
  where
    go 0 = return ()
    go i = do
      x <- Restore.get :: Get PackageDescription
      x `seq` go (i-1)

readPackageDescription :: String -> Int
readPackageDescription = length . (read :: String -> [PackageDescription])

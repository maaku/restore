-- Copyright (c) 2006-2019 The Binary Developers.
-- Copyright (c) 2019 The Restore Developers.
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0.  If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{-# LANGUAGE CPP, FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Trustworthy #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

#if MIN_VERSION_base(4,10,0)
{-# LANGUAGE MultiWayIf #-}
#endif

#if MIN_VERSION_base(4,8,0)
#define HAS_NATURAL
#define HAS_VOID
#endif

#if MIN_VERSION_base(4,7,0)
#define HAS_FIXED_CONSTRUCTOR
#endif

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Restore.Class
-- Copyright   : Lennart Kolmodin, Mark Friedenbach
-- License     : Mozilla Public License, v. 2.0 (see: LICENSE)
--
-- Maintainer  : Mark Friedenbach <mark@friedenbach.org>
-- Stability   : unstable
-- Portability : Known to work on GHC. Requires the FFI and some flexible instances.
--
-- Typeclass and instances for binary serialization.
--
-----------------------------------------------------------------------------

module Data.Restore.Class (

    -- * The Restore class
      Restore(..)

    -- * Support for generics
    , GRestoreGet(..)
    , GRestorePut(..)

    ) where

import Data.Word
import Data.Bits
import Data.Int
import Data.Complex (Complex(..))
#ifdef HAS_VOID
import Data.Void
#endif

import Data.Restore.Put
import Data.Restore.Get

#if ! MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Monoid (mempty)
#endif
import qualified Data.Monoid as Monoid
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
#if MIN_VERSION_base(4,8,0)
import Data.Functor.Identity (Identity (..))
#endif
#if MIN_VERSION_base(4,9,0)
import qualified Data.List.NonEmpty as NE
import qualified Data.Semigroup     as Semigroup
#endif
import Control.Monad

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder.Prim as Prim

import Data.List    (unfoldr, foldl')

-- And needed for the instances:
#if MIN_VERSION_base(4,10,0)
import Type.Reflection
import Type.Reflection.Unsafe
import Data.Kind (Type)
import GHC.Exts (TYPE, RuntimeRep(..), VecCount, VecElem)
#endif
import qualified Data.ByteString as B
#if MIN_VERSION_bytestring(0,10,4)
import qualified Data.ByteString.Short as BS
#endif
import qualified Data.Map        as Map
import qualified Data.Set        as Set
import qualified Data.IntMap     as IntMap
import qualified Data.IntSet     as IntSet
import qualified Data.Ratio      as R

import qualified Data.Tree as T

import Data.Array.Unboxed

import GHC.Generics

#ifdef HAS_NATURAL
import Numeric.Natural
#endif

import qualified Data.Fixed as Fixed

--
-- This isn't available in older Hugs or older GHC
--
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold

import GHC.Fingerprint

import Data.Version (Version(..))

------------------------------------------------------------------------

-- Factored into two classes because this makes GHC optimize the
-- instances faster.  This doesn't matter for builds of restore,
-- but it matters a lot for end-users who write 'instance Restore T'.
-- See also: https://ghc.haskell.org/trac/ghc/ticket/9630
class GRestorePut f where
    gput :: f t -> Put

class GRestoreGet f where
    gget :: Get (f t)

-- | The 'Restore' class provides 'put' and 'get', methods to encode and
-- decode a Haskell value to a lazy 'ByteString'. It mirrors the 'Read' and
-- 'Show' classes for textual representation of Haskell types, and is
-- suitable for serialising Haskell values to disk, over the network.
--
-- For decoding and generating simple external binary formats (e.g. C
-- structures), Restore may be used, but in general is not suitable
-- for complex protocols. Instead use the 'Put' and 'Get' primitives
-- directly.
--
-- Instances of Restore should satisfy the following property:
--
-- > decode . encode == id
--
-- That is, the 'get' and 'put' methods should be the inverse of each
-- other. A range of instances are provided for basic Haskell types.
--
class Restore t where
    -- | Encode a value in the Put monad.
    put :: t -> Put
    -- | Decode a value in the Get monad
    get :: Get t

    -- | Encode a list of values in the Put monad.
    -- The default implementation may be overridden to be more efficient
    -- but must still have the same encoding format.
    putList :: [t] -> Put
    putList = defaultPutList

    default put :: (Generic t, GRestorePut (Rep t)) => t -> Put
    put = gput . from

    default get :: (Generic t, GRestoreGet (Rep t)) => Get t
    get = to `fmap` gget

{-# INLINE defaultPutList #-}
defaultPutList :: Restore a => [a] -> Put
defaultPutList xs = put (length xs) <> mapM_ put xs

------------------------------------------------------------------------
-- Simple instances

#ifdef HAS_VOID
-- Void never gets written nor reconstructed since it's impossible to have a
-- value of that type

-- | @since 0.8.0.0
instance Restore Void where
    put     = absurd
    get     = mzero
#endif

-- The () type need never be written to disk: values of singleton type
-- can be reconstructed from the type alone
instance Restore () where
    put ()  = mempty
    get     = return ()

-- Bools are encoded as a byte in the range 0 .. 1
instance Restore Bool where
    put     = putWord8 . fromIntegral . fromEnum
    get     = getWord8 >>= toBool
      where
        toBool 0 = return False
        toBool 1 = return True
        toBool c = fail ("Could not map value " ++ show c ++ " to Bool")

-- Values of type 'Ordering' are encoded as a byte in the range 0 .. 2
instance Restore Ordering where
    put     = putWord8 . fromIntegral . fromEnum
    get     = getWord8 >>= toOrd
      where
        toOrd 0 = return LT
        toOrd 1 = return EQ
        toOrd 2 = return GT
        toOrd c = fail ("Could not map value " ++ show c ++ " to Ordering")

------------------------------------------------------------------------
-- Words and Ints

-- Words8s are written as bytes
instance Restore Word8 where
    put     = putWord8
    {-# INLINE putList #-}
    putList xs =
        put (length xs)
        <> putBuilder (Prim.primMapListFixed Prim.word8 xs)
    get     = getWord8

-- Words16s are written as 2 bytes in big-endian (network) order
instance Restore Word16 where
    put     = putWord16be
    {-# INLINE putList #-}
    putList xs =
        put (length xs)
        <> putBuilder (Prim.primMapListFixed Prim.word16BE xs)
    get     = getWord16be

-- Words32s are written as 4 bytes in big-endian (network) order
instance Restore Word32 where
    put     = putWord32be
    {-# INLINE putList #-}
    putList xs =
        put (length xs)
        <> putBuilder (Prim.primMapListFixed Prim.word32BE xs)
    get     = getWord32be

-- Words64s are written as 8 bytes in big-endian (network) order
instance Restore Word64 where
    put     = putWord64be
    {-# INLINE putList #-}
    putList xs =
        put (length xs)
        <> putBuilder (Prim.primMapListFixed Prim.word64BE xs)
    get     = getWord64be

-- Int8s are written as a single byte.
instance Restore Int8 where
    put     = putInt8
    {-# INLINE putList #-}
    putList xs =
        put (length xs)
        <> putBuilder (Prim.primMapListFixed Prim.int8 xs)
    get     = getInt8

-- Int16s are written as a 2 bytes in big endian format
instance Restore Int16 where
    put     = putInt16be
    {-# INLINE putList #-}
    putList xs =
        put (length xs)
        <> putBuilder (Prim.primMapListFixed Prim.int16BE xs)
    get     = getInt16be

-- Int32s are written as a 4 bytes in big endian format
instance Restore Int32 where
    put     = putInt32be
    {-# INLINE putList #-}
    putList xs =
        put (length xs)
        <> putBuilder (Prim.primMapListFixed Prim.int32BE xs)
    get     = getInt32be

-- Int64s are written as a 8 bytes in big endian format
instance Restore Int64 where
    put     = putInt64be
    {-# INLINE putList #-}
    putList xs =
        put (length xs)
        <> putBuilder (Prim.primMapListFixed Prim.int64BE xs)
    get     = getInt64be

------------------------------------------------------------------------

-- Words are are written as Word64s, that is, 8 bytes in big endian format
instance Restore Word where
    put     = putWord64be . fromIntegral
    {-# INLINE putList #-}
    putList xs =
        put (length xs)
        <> putBuilder (Prim.primMapListFixed Prim.word64BE (map fromIntegral xs))
    get     = liftM fromIntegral getWord64be

-- Ints are are written as Int64s, that is, 8 bytes in big endian format
instance Restore Int where
    put     = putInt64be . fromIntegral
    {-# INLINE putList #-}
    putList xs =
        put (length xs)
        <> putBuilder (Prim.primMapListFixed Prim.int64BE (map fromIntegral xs))
    get     = liftM fromIntegral getInt64be

------------------------------------------------------------------------
--
-- Portable, and pretty efficient, serialisation of Integer
--

-- Fixed-size type for a subset of Integer
type SmallInt = Int32

-- Integers are encoded in two ways: if they fit inside a SmallInt,
-- they're written as a byte tag, and that value.  If the Integer value
-- is too large to fit in a SmallInt, it is written as a byte array,
-- along with a sign and length field.

instance Restore Integer where

    {-# INLINE put #-}
    put n | n >= lo && n <= hi =
        putBuilder (Prim.primFixed (Prim.word8 Prim.>*< Prim.int32BE) (0, fromIntegral n))
     where
        lo = fromIntegral (minBound :: SmallInt) :: Integer
        hi = fromIntegral (maxBound :: SmallInt) :: Integer

    put n =
        putWord8 1
        <> put sign
        <> put (unroll (abs n))         -- unroll the bytes
     where
        sign = fromIntegral (signum n) :: Word8

    {-# INLINE get #-}
    get = do
        tag <- get :: Get Word8
        case tag of
            0 -> liftM fromIntegral (get :: Get SmallInt)
            _ -> do sign  <- get
                    bytes <- get
                    let v = roll bytes
                    return $! if sign == (1 :: Word8) then v else - v

-- | @since 0.8.0.0
#ifdef HAS_FIXED_CONSTRUCTOR
instance Restore (Fixed.Fixed a) where
  put (Fixed.MkFixed a) = put a
  get = Fixed.MkFixed `liftM` get
#else
instance forall a. Fixed.HasResolution a => Restore (Fixed.Fixed a) where
  -- Using undefined :: Maybe a as a proxy, as Data.Proxy is introduced only in base-4.7
  put x = put (truncate (x * fromInteger (Fixed.resolution (undefined :: Maybe a))) :: Integer)
  get = (\x -> fromInteger x / fromInteger (Fixed.resolution (undefined :: Maybe a))) `liftM` get
#endif

--
-- Fold and unfold an Integer to and from a list of its bytes
--
unroll :: (Integral a, Bits a) => a -> [Word8]
unroll = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

roll :: (Integral a, Bits a) => [Word8] -> a
roll   = foldl' unstep 0 . reverse
  where
    unstep a b = a `shiftL` 8 .|. fromIntegral b

#ifdef HAS_NATURAL
-- Fixed-size type for a subset of Natural
type NaturalWord = Word64

-- | @since 0.7.3.0
instance Restore Natural where
    {-# INLINE put #-}
    put n | n <= hi =
        putWord8 0
        <> put (fromIntegral n :: NaturalWord)  -- fast path
     where
        hi = fromIntegral (maxBound :: NaturalWord) :: Natural

    put n =
        putWord8 1
        <> put (unroll (abs n))         -- unroll the bytes

    {-# INLINE get #-}
    get = do
        tag <- get :: Get Word8
        case tag of
            0 -> liftM fromIntegral (get :: Get NaturalWord)
            _ -> do bytes <- get
                    return $! roll bytes
#endif

{-

--
-- An efficient, raw serialisation for Integer (GHC only)
--

-- TODO  This instance is not architecture portable.  GMP stores numbers as
-- arrays of machine sized words, so the byte format is not portable across
-- architectures with different endianness and word size.

import Data.ByteString.Base (toForeignPtr,unsafePackAddress, memcpy)
import GHC.Base     hiding (ord, chr)
import GHC.Prim
import GHC.Ptr (Ptr(..))
import GHC.IOBase (IO(..))

instance Restore Integer where
    put (S# i)    = putWord8 0 >> put (I# i)
    put (J# s ba) = do
        putWord8 1
        put (I# s)
        put (BA ba)

    get = do
        b <- getWord8
        case b of
            0 -> do (I# i#) <- get
                    return (S# i#)
            _ -> do (I# s#) <- get
                    (BA a#) <- get
                    return (J# s# a#)

instance Restore ByteArray where

    -- Pretty safe.
    put (BA ba) =
        let sz   = sizeofByteArray# ba   -- (primitive) in *bytes*
            addr = byteArrayContents# ba
            bs   = unsafePackAddress (I# sz) addr
        in put bs   -- write as a ByteString. easy, yay!

    -- Pretty scary. Should be quick though
    get = do
        (fp, off, n@(I# sz)) <- liftM toForeignPtr get      -- so decode a ByteString
        assert (off == 0) $ return $ unsafePerformIO $ do
            (MBA arr) <- newByteArray sz                    -- and copy it into a ByteArray#
            let to = byteArrayContents# (unsafeCoerce# arr) -- urk, is this safe?
            withForeignPtr fp $ \from -> memcpy (Ptr to) from (fromIntegral n)
            freezeByteArray arr

-- wrapper for ByteArray#
data ByteArray = BA  {-# UNPACK #-} !ByteArray#
data MBA       = MBA {-# UNPACK #-} !(MutableByteArray# RealWorld)

newByteArray :: Int# -> IO MBA
newByteArray sz = IO $ \s ->
  case newPinnedByteArray# sz s of { (# s', arr #) ->
  (# s', MBA arr #) }

freezeByteArray :: MutableByteArray# RealWorld -> IO ByteArray
freezeByteArray arr = IO $ \s ->
  case unsafeFreezeByteArray# arr s of { (# s', arr' #) ->
  (# s', BA arr' #) }

-}

instance (Restore a,Integral a) => Restore (R.Ratio a) where
    put r = put (R.numerator r) <> put (R.denominator r)
    get = liftM2 (R.%) get get

instance Restore a => Restore (Complex a) where
    {-# INLINE put #-}
    put (r :+ i) = put (r, i)
    {-# INLINE get #-}
    get = (\(r,i) -> r :+ i) <$> get

------------------------------------------------------------------------

-- Char is serialised as UTF-8
instance Restore Char where
    put = putCharUtf8
    putList str = put (length str) <> putStringUtf8 str
    get = do
        let getByte = liftM (fromIntegral :: Word8 -> Int) get
            shiftL6 = flip shiftL 6 :: Int -> Int
        w <- getByte
        r <- case () of
                _ | w < 0x80  -> return w
                  | w < 0xe0  -> do
                                    x <- liftM (xor 0x80) getByte
                                    return (x .|. shiftL6 (xor 0xc0 w))
                  | w < 0xf0  -> do
                                    x <- liftM (xor 0x80) getByte
                                    y <- liftM (xor 0x80) getByte
                                    return (y .|. shiftL6 (x .|. shiftL6
                                            (xor 0xe0 w)))
                  | otherwise -> do
                                x <- liftM (xor 0x80) getByte
                                y <- liftM (xor 0x80) getByte
                                z <- liftM (xor 0x80) getByte
                                return (z .|. shiftL6 (y .|. shiftL6
                                        (x .|. shiftL6 (xor 0xf0 w))))
        getChr r
      where
        getChr w
          | w <= 0x10ffff = return $! toEnum $ fromEnum w
          | otherwise = fail "Not a valid Unicode code point!"

------------------------------------------------------------------------
-- Instances for the first few tuples

instance (Restore a, Restore b) => Restore (a,b) where
    {-# INLINE put #-}
    put (a,b)           = put a <> put b
    {-# INLINE get #-}
    get                 = liftM2 (,) get get

instance (Restore a, Restore b, Restore c) => Restore (a,b,c) where
    {-# INLINE put #-}
    put (a,b,c)         = put a <> put b <> put c
    {-# INLINE get #-}
    get                 = liftM3 (,,) get get get

instance (Restore a, Restore b, Restore c, Restore d) => Restore (a,b,c,d) where
    {-# INLINE put #-}
    put (a,b,c,d)       = put a <> put b <> put c <> put d
    {-# INLINE get #-}
    get                 = liftM4 (,,,) get get get get

instance (Restore a, Restore b, Restore c, Restore d, Restore e) => Restore (a,b,c,d,e) where
    {-# INLINE put #-}
    put (a,b,c,d,e)     = put a <> put b <> put c <> put d <> put e
    {-# INLINE get #-}
    get                 = liftM5 (,,,,) get get get get get

--
-- and now just recurse:
--

instance (Restore a, Restore b, Restore c, Restore d, Restore e, Restore f)
        => Restore (a,b,c,d,e,f) where
    {-# INLINE put #-}
    put (a,b,c,d,e,f)   = put (a,(b,c,d,e,f))
    {-# INLINE get #-}
    get                 = do (a,(b,c,d,e,f)) <- get ; return (a,b,c,d,e,f)

instance (Restore a, Restore b, Restore c, Restore d, Restore e, Restore f, Restore g)
        => Restore (a,b,c,d,e,f,g) where
    {-# INLINE put #-}
    put (a,b,c,d,e,f,g) = put (a,(b,c,d,e,f,g))
    {-# INLINE get #-}
    get                 = do (a,(b,c,d,e,f,g)) <- get ; return (a,b,c,d,e,f,g)

instance (Restore a, Restore b, Restore c, Restore d, Restore e,
          Restore f, Restore g, Restore h)
        => Restore (a,b,c,d,e,f,g,h) where
    {-# INLINE put #-}
    put (a,b,c,d,e,f,g,h) = put (a,(b,c,d,e,f,g,h))
    {-# INLINE get #-}
    get                   = do (a,(b,c,d,e,f,g,h)) <- get ; return (a,b,c,d,e,f,g,h)

instance (Restore a, Restore b, Restore c, Restore d, Restore e,
          Restore f, Restore g, Restore h, Restore i)
        => Restore (a,b,c,d,e,f,g,h,i) where
    {-# INLINE put #-}
    put (a,b,c,d,e,f,g,h,i) = put (a,(b,c,d,e,f,g,h,i))
    {-# INLINE get #-}
    get                     = do (a,(b,c,d,e,f,g,h,i)) <- get ; return (a,b,c,d,e,f,g,h,i)

instance (Restore a, Restore b, Restore c, Restore d, Restore e,
          Restore f, Restore g, Restore h, Restore i, Restore j)
        => Restore (a,b,c,d,e,f,g,h,i,j) where
    {-# INLINE put #-}
    put (a,b,c,d,e,f,g,h,i,j) = put (a,(b,c,d,e,f,g,h,i,j))
    {-# INLINE get #-}
    get                       = do (a,(b,c,d,e,f,g,h,i,j)) <- get ; return (a,b,c,d,e,f,g,h,i,j)

------------------------------------------------------------------------
-- Container types

#if MIN_VERSION_base(4,8,0)
instance Restore a => Restore (Identity a) where
  put (Identity x) = put x
  get = Identity <$> get
#endif

instance Restore a => Restore [a] where
    put = putList
    get = do n <- get :: Get Int
             getMany n

-- | @'getMany' n@ get @n@ elements in order, without blowing the stack.
getMany :: Restore a => Int -> Get [a]
getMany n = go [] n
 where
    go xs 0 = return $! reverse xs
    go xs i = do x <- get
                 -- we must seq x to avoid stack overflows due to laziness in
                 -- (>>=)
                 x `seq` go (x:xs) (i-1)
{-# INLINE getMany #-}

instance (Restore a) => Restore (Maybe a) where
    put Nothing  = putWord8 0
    put (Just x) = putWord8 1 <> put x
    get = do
        w <- getWord8
        case w of
            0 -> return Nothing
            _ -> liftM Just get

instance (Restore a, Restore b) => Restore (Either a b) where
    put (Left  a) = putWord8 0 <> put a
    put (Right b) = putWord8 1 <> put b
    get = do
        w <- getWord8
        case w of
            0 -> liftM Left  get
            _ -> liftM Right get

------------------------------------------------------------------------
-- ByteStrings (have specially efficient instances)

instance Restore B.ByteString where
    put bs = put (B.length bs)
             <> putByteString bs
    get    = get >>= getByteString

--
-- Using old versions of fps, this is a type synonym, and non portable
--
-- Requires 'flexible instances'
--
instance Restore ByteString where
    put bs = put (fromIntegral (L.length bs) :: Int)
             <> putLazyByteString bs
    get    = get >>= getLazyByteString


#if MIN_VERSION_bytestring(0,10,4)
instance Restore BS.ShortByteString where
   put bs = put (BS.length bs)
            <> putShortByteString bs
   get = get >>= fmap BS.toShort . getByteString
#endif

------------------------------------------------------------------------
-- Maps and Sets

instance (Restore a) => Restore (Set.Set a) where
    put s = put (Set.size s) <> mapM_ put (Set.toAscList s)
    get   = liftM Set.fromDistinctAscList get

instance (Restore k, Restore e) => Restore (Map.Map k e) where
    put m = put (Map.size m) <> mapM_ put (Map.toAscList m)
    get   = liftM Map.fromDistinctAscList get

instance Restore IntSet.IntSet where
    put s = put (IntSet.size s) <> mapM_ put (IntSet.toAscList s)
    get   = liftM IntSet.fromDistinctAscList get

instance (Restore e) => Restore (IntMap.IntMap e) where
    put m = put (IntMap.size m) <> mapM_ put (IntMap.toAscList m)
    get   = liftM IntMap.fromDistinctAscList get

------------------------------------------------------------------------
-- Queues and Sequences

--
-- This is valid Hugs, but you need the most recent Hugs
--

instance (Restore e) => Restore (Seq.Seq e) where
    put s = put (Seq.length s) <> Fold.mapM_ put s
    get = do n <- get :: Get Int
             rep Seq.empty n get
      where rep xs 0 _ = return $! xs
            rep xs n g = xs `seq` n `seq` do
                           x <- g
                           rep (xs Seq.|> x) (n-1) g

------------------------------------------------------------------------
-- Floating point

instance Restore Double where
    put d = put (decodeFloat d)
    get   = do
        x <- get
        y <- get
        return $! encodeFloat x y

instance Restore Float where
    put f = put (decodeFloat f)
    get   =  do
        x <- get
        y <- get
        return $! encodeFloat x y

------------------------------------------------------------------------
-- Trees

instance (Restore e) => Restore (T.Tree e) where
    put (T.Node r s) = put r <> put s
    get = liftM2 T.Node get get

------------------------------------------------------------------------
-- Arrays

instance (Restore i, Ix i, Restore e) => Restore (Array i e) where
    put a =
        put (bounds a)
        <> put (rangeSize $ bounds a) -- write the length
        <> mapM_ put (elems a)        -- now the elems.
    get = do
        bs <- get
        n  <- get                  -- read the length
        xs <- getMany n            -- now the elems.
        return (listArray bs xs)

--
-- The IArray UArray e constraint is non portable. Requires flexible instances
--
instance (Restore i, Ix i, Restore e, IArray UArray e) => Restore (UArray i e) where
    put a =
        put (bounds a)
        <> put (rangeSize $ bounds a) -- now write the length
        <> mapM_ put (elems a)
    get = do
        bs <- get
        n  <- get
        xs <- getMany n
        return (listArray bs xs)

------------------------------------------------------------------------
-- Fingerprints

-- | @since 0.7.6.0
instance Restore Fingerprint where
    put (Fingerprint x1 x2) = put x1 <> put x2
    get = do
        x1 <- get
        x2 <- get
        return $! Fingerprint x1 x2

------------------------------------------------------------------------
-- Version

-- | @since 0.8.0.0
instance Restore Version where
    put (Version br tags) = put br <> put tags
    get = Version <$> get <*> get

------------------------------------------------------------------------
-- Data.Monoid datatypes

-- | @since 0.8.4.0
instance Restore a => Restore (Monoid.Dual a) where
  get = fmap Monoid.Dual get
  put = put . Monoid.getDual

-- | @since 0.8.4.0
instance Restore Monoid.All where
  get = fmap Monoid.All get
  put = put . Monoid.getAll

-- | @since 0.8.4.0
instance Restore Monoid.Any where
  get = fmap Monoid.Any get
  put = put . Monoid.getAny

-- | @since 0.8.4.0
instance Restore a => Restore (Monoid.Sum a) where
  get = fmap Monoid.Sum get
  put = put . Monoid.getSum

-- | @since 0.8.4.0
instance Restore a => Restore (Monoid.Product a) where
  get = fmap Monoid.Product get
  put = put . Monoid.getProduct

-- | @since 0.8.4.0
instance Restore a => Restore (Monoid.First a) where
  get = fmap Monoid.First get
  put = put . Monoid.getFirst

-- | @since 0.8.4.0
instance Restore a => Restore (Monoid.Last a) where
  get = fmap Monoid.Last get
  put = put . Monoid.getLast

#if MIN_VERSION_base(4,8,0)
-- | @since 0.8.4.0
instance Restore (f a) => Restore (Monoid.Alt f a) where
  get = fmap Monoid.Alt get
  put = put . Monoid.getAlt
#endif

#if MIN_VERSION_base(4,9,0)
------------------------------------------------------------------------
-- Data.Semigroup datatypes

-- | @since 0.8.4.0
instance Restore a => Restore (Semigroup.Min a) where
  get = fmap Semigroup.Min get
  put = put . Semigroup.getMin

-- | @since 0.8.4.0
instance Restore a => Restore (Semigroup.Max a) where
  get = fmap Semigroup.Max get
  put = put . Semigroup.getMax

-- | @since 0.8.4.0
instance Restore a => Restore (Semigroup.First a) where
  get = fmap Semigroup.First get
  put = put . Semigroup.getFirst

-- | @since 0.8.4.0
instance Restore a => Restore (Semigroup.Last a) where
  get = fmap Semigroup.Last get
  put = put . Semigroup.getLast

-- | @since 0.8.4.0
instance Restore a => Restore (Semigroup.Option a) where
  get = fmap Semigroup.Option get
  put = put . Semigroup.getOption

-- | @since 0.8.4.0
instance Restore m => Restore (Semigroup.WrappedMonoid m) where
  get = fmap Semigroup.WrapMonoid get
  put = put . Semigroup.unwrapMonoid

-- | @since 0.8.4.0
instance (Restore a, Restore b) => Restore (Semigroup.Arg a b) where
  get                     = liftM2 Semigroup.Arg get get
  put (Semigroup.Arg a b) = put a <> put b

------------------------------------------------------------------------
-- Non-empty lists

-- | @since 0.8.4.0
instance Restore a => Restore (NE.NonEmpty a) where
  get = do
      list <- get
      case list of
        [] -> fail "NonEmpty is empty!"
        x:xs -> pure (x NE.:| xs)
  put = put . NE.toList
#endif

------------------------------------------------------------------------
-- Typeable/Reflection

#if MIN_VERSION_base(4,10,0)

-- $typeable-instances
--
-- 'Restore' instances for GHC's "Type.Reflection", "Data.Typeable", and
-- kind-system primitives are only provided with @base-4.10.0@ (shipped with GHC
-- 8.2.1). In prior GHC releases some of these instances were provided by
-- 'GHCi.TH.Restore' in the @ghci@ package.
--
-- These include instances for,
--
-- * 'VecCount'
-- * 'VecElem'
-- * 'RuntimeRep'
-- * 'KindRep'
-- * 'TypeLitSort'
-- * 'TyCon'
-- * 'TypeRep'
-- * 'SomeTypeRep' (also known as 'Data.Typeable.TypeRep')
--

-- | @since 0.8.5.0
instance Restore VecCount where
    put = putWord8 . fromIntegral . fromEnum
    get = toEnum . fromIntegral <$> getWord8

-- | @since 0.8.5.0
instance Restore VecElem where
    put = putWord8 . fromIntegral . fromEnum
    get = toEnum . fromIntegral <$> getWord8

-- | @since 0.8.5.0
instance Restore RuntimeRep where
    put (VecRep a b)    = putWord8 0 >> put a >> put b
    put (TupleRep reps) = putWord8 1 >> put reps
    put (SumRep reps)   = putWord8 2 >> put reps
    put LiftedRep       = putWord8 3
    put UnliftedRep     = putWord8 4
    put IntRep          = putWord8 5
    put WordRep         = putWord8 6
    put Int64Rep        = putWord8 7
    put Word64Rep       = putWord8 8
    put AddrRep         = putWord8 9
    put FloatRep        = putWord8 10
    put DoubleRep       = putWord8 11
#if __GLASGOW_HASKELL__ >= 807
    put Int8Rep         = putWord8 12
    put Word8Rep        = putWord8 13
    put Int16Rep        = putWord8 14
    put Word16Rep       = putWord8 15
#endif

    get = do
        tag <- getWord8
        case tag of
          0  -> VecRep <$> get <*> get
          1  -> TupleRep <$> get
          2  -> SumRep <$> get
          3  -> pure LiftedRep
          4  -> pure UnliftedRep
          5  -> pure IntRep
          6  -> pure WordRep
          7  -> pure Int64Rep
          8  -> pure Word64Rep
          9  -> pure AddrRep
          10 -> pure FloatRep
          11 -> pure DoubleRep
#if __GLASGOW_HASKELL__ >= 807
          12 -> pure Int8Rep
          13 -> pure Word8Rep
          14 -> pure Int16Rep
          15 -> pure Word16Rep
#endif
          _  -> fail "GHCi.TH.Restore.putRuntimeRep: invalid tag"

-- | @since 0.8.5.0
instance Restore TyCon where
    put tc = do
        put (tyConPackage tc)
        put (tyConModule tc)
        put (tyConName tc)
        put (tyConKindArgs tc)
        put (tyConKindRep tc)
    get = mkTyCon <$> get <*> get <*> get <*> get <*> get

-- | @since 0.8.5.0
instance Restore KindRep where
    put (KindRepTyConApp tc k) = putWord8 0 >> put tc >> put k
    put (KindRepVar bndr) = putWord8 1 >> put bndr
    put (KindRepApp a b) = putWord8 2 >> put a >> put b
    put (KindRepFun a b) = putWord8 3 >> put a >> put b
    put (KindRepTYPE r) = putWord8 4 >> put r
    put (KindRepTypeLit sort r) = putWord8 5 >> put sort >> put r

    get = do
        tag <- getWord8
        case tag of
          0 -> KindRepTyConApp <$> get <*> get
          1 -> KindRepVar <$> get
          2 -> KindRepApp <$> get <*> get
          3 -> KindRepFun <$> get <*> get
          4 -> KindRepTYPE <$> get
          5 -> KindRepTypeLit <$> get <*> get
          _ -> fail "GHCi.TH.Restore.putKindRep: invalid tag"

-- | @since 0.8.5.0
instance Restore TypeLitSort where
    put TypeLitSymbol = putWord8 0
    put TypeLitNat = putWord8 1
    get = do
        tag <- getWord8
        case tag of
          0 -> pure TypeLitSymbol
          1 -> pure TypeLitNat
          _ -> fail "GHCi.TH.Restore.putTypeLitSort: invalid tag"

putTypeRep :: TypeRep a -> Put
-- Special handling for TYPE, (->), and RuntimeRep due to recursive kind
-- relations.
-- See Note [Mutually recursive representations of primitive types]
putTypeRep rep  -- Handle Type specially since it's so common
  | Just HRefl <- rep `eqTypeRep` (typeRep :: TypeRep Type)
  = put (0 :: Word8)
putTypeRep (Con' con ks) = do
    put (1 :: Word8)
    put con
    put ks
putTypeRep (App f x) = do
    put (2 :: Word8)
    putTypeRep f
    putTypeRep x
putTypeRep (Fun arg res) = do
    put (3 :: Word8)
    putTypeRep arg
    putTypeRep res
putTypeRep _ = error "GHCi.TH.Restore.putTypeRep: Impossible"

getSomeTypeRep :: Get SomeTypeRep
getSomeTypeRep = do
    tag <- get :: Get Word8
    case tag of
        0 -> return $ SomeTypeRep (typeRep :: TypeRep Type)
        1 -> do con <- get :: Get TyCon
                ks <- get :: Get [SomeTypeRep]
                return $ SomeTypeRep $ mkTrCon con ks
        2 -> do SomeTypeRep f <- getSomeTypeRep
                SomeTypeRep x <- getSomeTypeRep
                case typeRepKind f of
                  Fun arg res ->
                      case arg `eqTypeRep` typeRepKind x of
                        Just HRefl -> do
                            case typeRepKind res `eqTypeRep` (typeRep :: TypeRep Type) of
                                Just HRefl -> return $ SomeTypeRep $ mkTrApp f x
                                _ -> failure "Kind mismatch" []
                        _ -> failure "Kind mismatch"
                             [ "Found argument of kind:      " ++ show (typeRepKind x)
                             , "Where the constructor:       " ++ show f
                             , "Expects an argument of kind: " ++ show arg
                             ]
                  _ -> failure "Applied non-arrow type"
                       [ "Applied type: " ++ show f
                       , "To argument:  " ++ show x
                       ]
        3 -> do SomeTypeRep arg <- getSomeTypeRep
                SomeTypeRep res <- getSomeTypeRep
                if
                  | App argkcon _ <- typeRepKind arg
                  , App reskcon _ <- typeRepKind res
                  , Just HRefl <- argkcon `eqTypeRep` tYPErep
                  , Just HRefl <- reskcon `eqTypeRep` tYPErep
                  -> return $ SomeTypeRep $ Fun arg res
                  | otherwise -> failure "Kind mismatch" []
        _ -> failure "Invalid SomeTypeRep" []
  where
    tYPErep :: TypeRep TYPE
    tYPErep = typeRep

    failure description info =
        fail $ unlines $ [ "GHCi.TH.Restore.getSomeTypeRep: "++description ]
                      ++ map ("    "++) info

instance Typeable a => Restore (TypeRep (a :: k)) where
    put = putTypeRep
    get = do
        SomeTypeRep rep <- getSomeTypeRep
        case rep `eqTypeRep` expected of
          Just HRefl -> pure rep
          Nothing    -> fail $ unlines
                        [ "GHCi.TH.Restore: Type mismatch"
                        , "    Deserialized type: " ++ show rep
                        , "    Expected type:     " ++ show expected
                        ]
     where expected = typeRep :: TypeRep a

instance Restore SomeTypeRep where
    put (SomeTypeRep rep) = putTypeRep rep
    get = getSomeTypeRep
#endif


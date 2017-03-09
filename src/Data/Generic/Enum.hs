{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Generic.Enum (
  Enum, succ, pred, toEnum, fromEnum, numStepsBetween,
  DefaultEnum, defaultFromEnum, defaultToEnum,
  EnumFromTo, enumFromTo, enumFromThenTo, enumFromThenCount, enumFromCount, enumFromStepTo, enumFromStepCount,
  EnumFrom, enumFrom, enumFromThen, enumFromStep,
  Element
  ) where

import qualified Prelude
import Prelude hiding (Enum, fromEnum, toEnum, succ, pred, enumFrom, enumFromThen, enumFromTo, enumFromThenTo)

import Data.Int
import Data.Word
import Data.Char
import System.IO
import GHC.Generics
import Foreign.C.Types
import Foreign.Ptr
import System.Posix.Types
import Numeric.Natural
import GHC.RTS.Flags
import Data.Ratio
import Data.Fixed
import Data.Semigroup
import Data.Functor.Identity
import Data.Proxy
import Data.Type.Equality
import Data.Coerce
import Data.Type.Coercion
import Data.Monoid (Alt(Alt), getAlt)
import Data.Functor.Const
import qualified GHC.Enum

import Data.Array (Array, listArray, Ix, elems)
import Data.List (genericLength)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS

{-|
The generic 'Enum' class. Firstly, this class just deals with 'fromEnum', 'toEnum' type functions,
not the list generating functions like 'Prelude.enumFrom' and 'Prelude.enumFromTo'
the normal 'Prelude.Enum' has.

This class has a number of defaults for making defining both existing
-}
class (Num (EnumNumT a), Integral (EnumIntegralT a)) => Enum a where
  type EnumNumT a
  type EnumNumT a = Int

  type EnumIntegralT a
  type EnumIntegralT a = EnumNumT a

  succ :: a -> a
  succ = toEnum . (+ 1)  . fromEnum

  pred :: a -> a
  pred = toEnum . (subtract 1)  . fromEnum

  toEnum :: EnumNumT a -> a
  default toEnum :: (DefaultEnum a (EnumNumT a)) => EnumNumT a -> a
  toEnum = defaultToEnum

  fromEnum :: a -> EnumNumT a
  default fromEnum :: (DefaultEnum a (EnumNumT a)) => a -> EnumNumT a
  fromEnum = defaultFromEnum

  numStepsBetween :: a -> a -> EnumNumT a -> EnumIntegralT a
  default numStepsBetween :: (e ~ EnumNumT a, e ~ EnumIntegralT a) =>  a -> a -> e -> e
  numStepsBetween x y stepSize = max ((fromEnum y - fromEnum x) `div` stepSize + 1) 0

class DefaultEnum a b where
  defaultFromEnum :: a -> b
  defaultToEnum :: b -> a

instance (Prelude.Enum a) => DefaultEnum a Int where
  defaultFromEnum = Prelude.fromEnum
  defaultToEnum = Prelude.toEnum

instance DefaultEnum a a where
  defaultFromEnum = id
  defaultToEnum = id

instance {-# OVERLAPPING #-} DefaultEnum Int Int where
  defaultFromEnum = id
  defaultToEnum = id

instance Enum Bool
instance Enum Char
instance Enum Int

instance Enum Int8 where
  type EnumNumT Int8 = Int8

instance Enum Int16 where
  type EnumNumT Int16 = Int16

instance Enum Int32 where
  type EnumNumT Int32 = Int32

instance Enum Int64 where
  type EnumNumT Int64 = Int64

instance Enum Integer where
  type EnumNumT Integer = Integer

instance Enum Ordering

instance Enum Word8 where
  type EnumNumT Word8 = Word8

instance Enum Word16 where
  type EnumNumT Word16 = Word16

instance Enum Word32 where
  type EnumNumT Word32 = Word32

instance Enum Word64 where
  type EnumNumT Word64 = Word64

instance Enum ()

instance Enum GeneralCategory
instance Enum IOMode
instance Enum DecidedStrictness
instance Enum SourceStrictness
instance Enum SourceUnpackedness
instance Enum Associativity
instance Enum SeekMode

instance Enum CUIntMax where
  type EnumNumT CUIntMax = CUIntMax

instance Enum CIntMax where
  type EnumNumT CIntMax = CIntMax

instance Enum CUIntPtr where
  type EnumNumT CUIntPtr = CUIntPtr

instance Enum CIntPtr where
  type EnumNumT CIntPtr = CIntPtr

instance Enum CSUSeconds
instance Enum CUSeconds
instance Enum CTime
instance Enum CClock

instance Enum CSigAtomic where
  type EnumNumT CSigAtomic = CSigAtomic

instance Enum CWchar where
  type EnumNumT CWchar = CWchar

instance Enum CSize where
  type EnumNumT CSize = CSize

instance Enum CPtrdiff where
  type EnumNumT CPtrdiff = CPtrdiff

instance Enum CDouble
instance Enum CFloat
instance Enum CULLong where
  type EnumNumT CULLong = CULLong

instance Enum CLLong where
  type EnumNumT CLLong = CLLong

instance Enum CULong where
  type EnumNumT CULong = CULong

instance Enum CLong where
  type EnumNumT CLong = CLong

instance Enum CUInt where
  type EnumNumT CUInt = CUInt

instance Enum CInt where
  type EnumNumT CInt = CInt

instance Enum CUShort where
  type EnumNumT CUShort = CUShort

instance Enum CShort where
  type EnumNumT CShort = CShort

instance Enum CUChar where
  type EnumNumT CUChar = CUChar

instance Enum CSChar where
  type EnumNumT CSChar = CSChar

instance Enum CChar where
  type EnumNumT CChar = CChar

instance Enum IntPtr where
  type EnumNumT IntPtr = IntPtr

instance Enum WordPtr where
  type EnumNumT WordPtr = WordPtr

instance Enum Fd where
  type EnumNumT Fd = Fd

instance Enum CRLim where
  type EnumNumT CRLim = CRLim

instance Enum CTcflag where
  type EnumNumT CTcflag = CTcflag

instance Enum CSpeed
instance Enum CCc

instance Enum CUid where
  type EnumNumT CUid = CUid

instance Enum CNlink where
  type EnumNumT CNlink = CNlink

instance Enum CGid where
  type EnumNumT CGid = CGid

instance Enum CSsize where
  type EnumNumT CSsize = CSsize

instance Enum CPid where
  type EnumNumT CPid = CPid

instance Enum COff where
  type EnumNumT COff = COff

instance Enum CMode where
  type EnumNumT CMode = CMode

instance Enum CIno where
  type EnumNumT CIno = CIno

instance Enum CDev where
  type EnumNumT CDev = CDev

instance Enum Natural where
  type EnumNumT Natural = Natural

instance Enum DoTrace
instance Enum DoHeapProfile
instance Enum DoCostCentres
instance Enum GiveGCStats

instance (Integral a) => Enum (Ratio a) where
  type EnumNumT (Ratio a) = Ratio a
  type EnumIntegralT (Ratio a) = a
  numStepsBetween x y stepSize = max (floor ((y - x) / stepSize + 1)) 0

instance (HasResolution a) => Enum (Fixed a) where
  type EnumNumT (Fixed a) = Fixed a
  type EnumIntegralT (Fixed a) = Integer
  numStepsBetween x y stepSize = max (floor ((y - x) / stepSize + 1)) 0

instance Enum a => Enum (WrappedMonoid a) where
  type EnumNumT (WrappedMonoid a) = EnumNumT a
  type EnumIntegralT (WrappedMonoid a) = EnumIntegralT a
  succ (WrapMonoid a) = WrapMonoid (succ a)
  pred (WrapMonoid a) = WrapMonoid (pred a)
  toEnum = WrapMonoid . toEnum
  fromEnum = fromEnum . unwrapMonoid
  numStepsBetween (WrapMonoid x) (WrapMonoid y) stepSize = numStepsBetween x y stepSize

instance Enum a => Enum (Last a) where
  type EnumNumT (Last a) = EnumNumT a
  type EnumIntegralT (Last a) = EnumIntegralT a
  succ (Last a) = Last (succ a)
  pred (Last a) = Last (pred a)
  toEnum = Last . toEnum
  fromEnum = fromEnum . getLast
  numStepsBetween (Last x) (Last y) stepSize = numStepsBetween x y stepSize

instance Enum a => Enum (First a) where
  type EnumNumT (First a) = EnumNumT a
  type EnumIntegralT (First a) = EnumIntegralT a
  succ (First a) = First (succ a)
  pred (First a) = First (pred a)
  toEnum = First . toEnum
  fromEnum = fromEnum . getFirst
  numStepsBetween (First x) (First y) stepSize = numStepsBetween x y stepSize

instance Enum a => Enum (Max a) where
  type EnumNumT (Max a) = EnumNumT a
  type EnumIntegralT (Max a) = EnumIntegralT a
  succ (Max a) = Max (succ a)
  pred (Max a) = Max (pred a)
  toEnum = Max . toEnum
  fromEnum = fromEnum . getMax
  numStepsBetween (Max x) (Max y) stepSize = numStepsBetween x y stepSize

instance Enum a => Enum (Min a) where
  type EnumNumT (Min a) = EnumNumT a
  type EnumIntegralT (Min a) = EnumIntegralT a
  succ (Min a) = Min (succ a)
  pred (Min a) = Min (pred a)
  toEnum = Min . toEnum
  fromEnum = fromEnum . getMin
  numStepsBetween (Min x) (Min y) stepSize = numStepsBetween x y stepSize

instance Enum a => Enum (Identity a) where
  type EnumNumT (Identity a) = EnumNumT a
  type EnumIntegralT (Identity a) = EnumIntegralT a
  succ (Identity a) = Identity (succ a)
  pred (Identity a) = Identity (pred a)
  toEnum = Identity . toEnum
  fromEnum = fromEnum . runIdentity
  numStepsBetween (Identity x) (Identity y) stepSize = numStepsBetween x y stepSize

instance Enum (Proxy s)
instance ((~) a b) => Enum ((:~:) a b)
instance (Coercible a b) => Enum (Coercion a b)

instance Enum (f a) => Enum (Alt f a) where
  type EnumNumT (Alt f a) = EnumNumT (f a)
  type EnumIntegralT (Alt f a) = EnumIntegralT (f a)
  succ (Alt x) = Alt (succ x)
  pred (Alt x) = Alt (pred x)
  toEnum = Alt . toEnum
  fromEnum = fromEnum . getAlt
  numStepsBetween (Alt x) (Alt y) stepSize = numStepsBetween x y stepSize

instance Enum a => Enum (Const a b) where
  type EnumNumT (Const a b) = EnumNumT a
  type EnumIntegralT (Const a b) = EnumIntegralT a
  succ (Const x) = Const (succ x)
  pred (Const x) = Const (pred x)
  toEnum = Const . toEnum
  fromEnum = fromEnum . getConst
  numStepsBetween (Const x) (Const y) stepSize = numStepsBetween x y stepSize

type family Element a

class Enum (Element a) => EnumFromTo a where
  enumFromTo :: Element a -> Element a -> a
  enumFromTo x = enumFromStepTo x 1

  enumFromThenTo :: Element a -> Element a -> Element a -> a
  enumFromThenTo x next_x y = enumFromStepTo x (fromEnum next_x - fromEnum x) y

  enumFromThenCount :: Element a -> Element a -> EnumIntegralT (Element a) -> a
  enumFromThenCount x next_x = enumFromStepCount x (fromEnum next_x - fromEnum x)

  enumFromCount :: Element a -> EnumIntegralT (Element a) -> a
  enumFromCount x = enumFromStepCount x 1

  enumFromStepTo :: Element a -> EnumNumT (Element a) -> Element a -> a
  enumFromStepTo x stepSize y = enumFromStepCount x stepSize (numStepsBetween x y stepSize)

  enumFromStepCount :: Element a -> EnumNumT (Element a) -> EnumIntegralT (Element a) -> a

class Enum (Element a) => EnumFrom a where
  enumFrom :: Element a -> a
  enumFrom x = enumFromStep x 1

  enumFromThen :: Element a -> Element a -> a
  enumFromThen x next_x = enumFromStep x (fromEnum next_x - fromEnum x)

  enumFromStep :: Element a -> EnumNumT (Element a) -> a
  default enumFromStep :: (Bounded (EnumIntegralT (Element a)), EnumFromTo a) => Element a -> EnumNumT (Element a) -> a
  enumFromStep x stepSize = enumFromStepCount x stepSize maxBound

type instance Element [a] = a

instance Enum a => EnumFromTo [a] where
  enumFromStepCount x stepSize count = go (fromEnum x) count where
    go x n = case n of
      0 -> []
      _ -> (toEnum x):(go (x + stepSize) (n-1))

instance Enum a => EnumFrom [a] where
  enumFromStep x stepSize = go (fromEnum x) where
    go x = (toEnum x):(go (x + stepSize))

type instance Element (Array i e) = e

instance (Enum e, Ix i, Num i) => EnumFromTo (Array i e) where
  enumFromStepCount x stepSize count = listArray (fromIntegral 0, fromIntegral (count - 1)) (enumFromStepCount x stepSize count)

type instance Element BS.ByteString = Word8
type instance Element BSL.ByteString = Word8
type instance Element BSS.ShortByteString = Word8

byteStringUnfoldrN f x stepSize count = let (r, _) = f (fromIntegral count) (\i -> Just (toEnum i, i + stepSize)) (fromEnum x) in r

instance EnumFromTo BS.ByteString where
  enumFromStepCount = byteStringUnfoldrN BS.unfoldrN

instance EnumFromTo BSL.ByteString where
  enumFromStepCount x stepSize count = BSL.unfoldr f (fromEnum x, count) where
    f (i, n) = case n of
      0 -> Nothing
      _ -> Just (toEnum i, (i + stepSize, n - 1))

instance EnumFromTo BSS.ShortByteString where
  enumFromStepCount x stepSize count = BSS.toShort (byteStringUnfoldrN BS.unfoldrN x stepSize count)


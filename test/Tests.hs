{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Prelude hiding (
  Enum, fromEnum, toEnum, succ, pred, enumFrom, enumFromThen, enumFromTo, enumFromThenTo
  )
import qualified Prelude as P
import Data.Generic.Enum

import Data.Generic.Enum (
  Element
  )

import Data.Proxy (Proxy(Proxy))
import Data.Ratio (Rational, (%))
import Data.Array (Array)
import Data.Word (Word8)

import qualified Data.ByteString as B

import Test.Hspec (hspec, it, shouldBe)

n1_int :: Int
n1_int = 2

n2_int :: Int
n2_int = 5

n1_rational :: Rational
n1_rational = 7 % 3

n2_rational :: Rational
n2_rational = 6 % 1

_a :: Word8
_a = P.toEnum (P.fromEnum 'a')

_e :: Word8
_e = P.toEnum (P.fromEnum 'e')

main = hspec $ do
  it "enumFromTo integer test" $ enumFromTo n1_int n2_int `shouldBe` P.enumFromTo n1_int n2_int
  it "enumFromTo rational test" $ sum ((enumFromTo n1_rational n2_rational) :: [Rational]) `shouldBe` (46 % 3)
  it "enumFromTo bytestring test" $ enumFromTo _a _e `shouldBe` B.pack [_a.._e]

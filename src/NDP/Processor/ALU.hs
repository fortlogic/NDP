{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise -fplugin GHC.TypeLits.KnownNat.Solver #-}
module NDP.Processor.ALU (ALUMode, ALUOp, alu) where

import Data.Singletons.Prelude
import Data.Word
import CLaSH.Prelude hiding (Left,Right)

{-
| Mode     | Description                             |
|----------+-----------------------------------------|
| Signed   | Signed twos-complement integers         |
| Unsigned | Unsigned binary integers                |
| BCD      | Binary coded decimal                    |
| Raw      | Raw bits, like unsigned but never traps |


| Operation   | Modes | Inputs | Outputs | Description            | Traps               |
|-------------+-------+--------+---------+------------------------+---------------------|
| Zero        | All   |    N/A |       1 | O = 0                  | N/A                 |
| Left        | All   |      2 |       1 | O = L                  | N/A                 |
| Right       | All   |      2 |       1 | O = R                  | N/A                 |
| Not         | Raw   |      1 |       1 | O = NOT L              | N/A                 |
| And         | Raw   |      2 |       1 | O = L AND R            | N/A                 |
| Or          | Raw   |      2 |       1 | O = L OR R             | N/A                 |
| Xor         | Raw   |      2 |       1 | O = L XOR R            | N/A                 |
| Increment   | All   |      1 |       1 | O = L + 1              | on overflow         |
| Decrement   | All   |      1 |       1 | O = L - 1              | on underflow        |
| Add         | All   |      2 |       1 | O = L + R              | on overflow         |
| Subtract    | All   |      2 |       1 | O = L - R              | on underflow        |
| Multiply    | All   |      2 |       2 | (O1, O2) = L * R       | on overflow         |
| Divide      | All   |      2 |       2 | O1 = L / R, O2 = L % R | on division by zero |
| ShiftLeft   | All   |        |         |                        |                     |
| ShiftRight  | All   |        |         |                        |                     |
| RotateLeft  | All   |        |         |                        |                     |
| RotateRight | All   |        |         |                        |                     |
| Unbox       | Raw   |      1 |       2 | O1 = Tag, O2 = Data    | N/A                 |
| UnboxTag    | Raw   |      1 |       1 | O = Tag                | N/A                 |
| UnboxData   | Raw   |      1 |       1 | O = Data               | N/A                 |
| Box         | Raw   |      2 |       1 | O = L.Tag + R.Data     | N/A                 |
-}

type ALUWord = Word32
type ALUResult = (ALUWord, ALUWord, ALUFault)

data ALUMode = Signed | Unsigned | Raw | BCD deriving (Read, Show, Eq)

data ALUOp = Zero
           | Left
           | Right
           | Not
           | And
           | Or
           | Xor
           | PopCount
           | Increment
           | Decrement
           | Negate
           | Add
           | Subtract
           | Multiply
           | Divide -- if ALU can have two outputs then this encompasses quotient and remainder
           | Quotient
           | Remainder
           | ShiftUp
           | ShiftDown
           | RotateUp
           | RotateDown
           deriving (Show, Read, Eq)

data ALUFault = Faultless | Overflow | Underflow | DivZero | BadMode deriving (Show, Read, Eq)

alu :: ALUMode -> ALUOp -> ALUWord -> ALUWord -> ALUResult
alu _ Zero  _ _ = unaryResult 0
alu _ Left  l _ = unaryResult l
alu _ Right _ r = unaryResult r
alu m Not   l _ = rawMode m $ unaryResult (bitwise not l)
alu m And   l r = rawMode m $ unaryResult (bitwise2 (&&) l r)
alu m Or    l r = rawMode m $ unaryResult (bitwise2 (||) l r)
alu m Xor   l r = rawMode m $ unaryResult (bitwise2 xor l r)

-- Returns the result if the modes match, otherwise fail with `BadMode`.
requireMode :: ALUMode -> ALUMode -> ALUResult -> ALUResult
requireMode m m' r
  | m == m'   = r
  | otherwise = aluFail BadMode

-- Fails with `BadMode` if the provided mode isn't `Raw`.
rawMode :: ALUMode -> ALUResult -> ALUResult
rawMode m r = requireMode Raw m r

-- Constructs a successful single-valued ALU response.
unaryResult :: ALUWord -> ALUResult
unaryResult w = (w, undefined, Faultless)

-- Constructs a faulting ALU response.
aluFail :: ALUFault -> ALUResult
aluFail f = (undefined, undefined, f)

packV :: (BitPack w, KnownNat (BitSize w)) => w -> Vec (BitSize w) Bool
packV = unpack . pack

unpackV :: (BitPack w, KnownNat (BitSize w)) => Vec (BitSize w) Bool -> w
unpackV = bitCoerce

bitwise :: (BitPack w, KnownNat (BitSize w)) => (Bool -> Bool) -> w -> w
bitwise f w = unpackV w'
  where w' = f <$> packV w

bitwise2 :: (BitPack w, KnownNat (BitSize w)) => (Bool -> Bool -> Bool) -> w -> w -> w
bitwise2 f w1 w2 = unpackV wR
  where wR = f <$> packV w1 <*> packV w2

bv2t :: KnownNat n => BitVector (2 ^ n) -> RTree n Bit
bv2t = v2t . unpack

data Width2Index (f :: TyFun Nat *) :: *
type instance Apply Width2Index n = Index ((2^n)+1)

popcount :: KnownNat n => BitVector (2 ^ n) -> Index ((2^n)+1)
popcount bv = tdfold (Proxy @Width2Index) fromIntegral (\_ a b -> plus a b) rt
  where rt = bv2t bv


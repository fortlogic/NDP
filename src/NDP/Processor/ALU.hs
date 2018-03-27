module NDP.Processor.ALU (ALUMode, ALUOp, alu) where

import Data.Word

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

data ALUMode = Signed | Unsigned | Raw | BCD

data ALUOp = Zero
           | Left
           | Right
           | Not
           | And
           | Or
           | Xor
           | Increment
           | Decrement
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

alu :: ALUMode -> ALUOp -> Word32 -> Word32 -> (Word32, Word32)
alu _ Zero _ _ = (0,0)

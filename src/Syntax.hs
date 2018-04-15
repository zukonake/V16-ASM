module Syntax where

import Data.Word

data Opcode = Nop
            | Jump
            | Term
            | Call
            | Ret
            | Push
            | Pop
            | Move
            | Copy
            | Swap
            | Ifeq
            | Ifnq
            | Ifgt
            | Iflt
            | Ifgq
            | Iflq
            | Neg
            | Or
            | And
            | Xor
            | Rshf
            | Lshf
            | Swpb
            | Add
            | Sub
            | Mul
            | Div
            | Mod
            | Adbi
            | Adbo
            | Adwi
            | Adwo

data ModeKind = Literal
              | Register
              | RegisterPostIncr
              | RegisterPreIncr
              | PcOffsetPositive
              | PcOffsetNegative
              | CarryFlag

data ModeDir = Direct
             | Indirect

data Mode = Mode ModeKind ModeDir

data Port = Port Mode (Maybe Word16)

data Node = PlainData [Word16]
          | Instruction Opcode (Maybe Port) (Maybe Port)

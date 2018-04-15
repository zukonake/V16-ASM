module Syntax where

import Data.Word

data Opcode = Nop
            | Jmp
            | Ter
            | Cll
            | Ret
            | Psh
            | Pop
            | Mov
            | Cpy
            | Swp
            | Ieq
            | Inq
            | Igt
            | Ilt
            | Igq
            | Ilq
            | Neg
            | Or
            | And
            | Xor
            | Rsh
            | Lsh
            | Spb
            | Add
            | Sub
            | Mul
            | Div
            | Mod
            | Aib
            | Aob
            | Aiw
            | Aow

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

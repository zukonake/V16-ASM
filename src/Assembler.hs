{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Assembler(assembleNodes) where

import Data.Maybe
import Data.Word
import Data.Bits

import Syntax

assembleNodes :: [Node] -> [Word16]
assembleNodes = (concat . map assemble)

class Assemble a b where
    assemble :: a -> b

instance Assemble Opcode Word16 where
    assemble Nop = 0x00;
    assemble Jmp = 0x01;
    assemble Ter = 0x02;
    assemble Cll = 0x03;
    assemble Ret = 0x04;
    assemble Mov = 0x10;
    assemble Cpy = 0x11;
    assemble Swp = 0x12;
    assemble Ieq = 0x20;
    assemble Inq = 0x21;
    assemble Igt = 0x22;
    assemble Ilt = 0x23;
    assemble Igq = 0x24;
    assemble Ilq = 0x25;
    assemble Neg = 0x30;
    assemble Or  = 0x31;
    assemble And = 0x32;
    assemble Xor = 0x33;
    assemble Rsh = 0x34;
    assemble Lsh = 0x35;
    assemble Spb = 0x36;
    assemble Add = 0x40;
    assemble Sub = 0x41;
    assemble Mul = 0x42;
    assemble Div = 0x43;
    assemble Mod = 0x44;
    assemble Aib = 0x50;
    assemble Aob = 0x51;
    assemble Aiw = 0x52;
    assemble Aow = 0x53;

instance Assemble ModeKind Word16 where
    assemble Literal          = 0x0;
    assemble Register         = 0x1;
    assemble RegisterPostIncr = 0x2;
    assemble RegisterPreIncr  = 0x3;
    assemble PcOffsetPositive = 0x4;
    assemble PcOffsetNegative = 0x5;
    assemble CarryFlag        = 0x6;

instance Assemble ModeDir Word16 where
    assemble Direct   = 0x0
    assemble Indirect = 0x1

instance Assemble Mode Word16 where
    assemble (Mode kind indirect) = ((assemble indirect) `shift` 3) .|. (assemble kind)

instance Assemble Port (Word16, Maybe Word16) where
    assemble (Port mode val) = (assemble mode, val)

instance Assemble Node [Word16] where
    assemble (PlainData val) = val
    assemble (Instruction opcode portX portY) = do
        (((assemble opcode) `shift` 8) .|. (a `shift` 4) .|. b):(maybe [] (:(maybe [] (:[]) y)) x)
            where (a, x) = fromMaybe (0x00, Nothing) (fmap assemble portX)
                  (b, y) = fromMaybe (0x00, Nothing) (fmap assemble portY)


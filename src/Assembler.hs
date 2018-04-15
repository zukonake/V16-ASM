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
    assemble Nop  = 0x00;
    assemble Jump = 0x01;
    assemble Term = 0x02;
    assemble Call = 0x03;
    assemble Ret  = 0x04;
    assemble Push = 0x05;
    assemble Pop  = 0x06;
    assemble Move = 0x10;
    assemble Copy = 0x11;
    assemble Swap = 0x12;
    assemble Ifeq = 0x20;
    assemble Ifnq = 0x21;
    assemble Ifgt = 0x22;
    assemble Iflt = 0x23;
    assemble Ifgq = 0x24;
    assemble Iflq = 0x25;
    assemble Neg  = 0x30;
    assemble Or   = 0x31;
    assemble And  = 0x32;
    assemble Xor  = 0x33;
    assemble Rshf = 0x34;
    assemble Lshf = 0x35;
    assemble Swpb = 0x36;
    assemble Add  = 0x40;
    assemble Sub  = 0x41;
    assemble Mul  = 0x42;
    assemble Div  = 0x43;
    assemble Mod  = 0x44;
    assemble Adbi = 0x50;
    assemble Adbo = 0x51;
    assemble Adwi = 0x52;
    assemble Adwo = 0x53;

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


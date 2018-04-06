module Ast where

import Data.Maybe
import Data.Word
import Data.Bits

import ReadMaybe
import Assemble

data Opcode = Nop
            | Jmp
            | Ter
            | Cll
            | Ret
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
            | Adi
            | Ado

data ModeKind = Memory
              | Register
              | RegisterPostIncr
              | RegisterPreIncr
              | PcOffsetPositive
              | PcOffsetNegative
              | CarryFlag

data Direction = Direct
               | Indirect

data Mode = Mode ModeKind Direction

data Port = Port Mode (Maybe Word16)

data Line = PlainData [Word16]
          | Instruction Opcode (Maybe Port) (Maybe Port)

instance Assemble Opcode where
    assemble Nop = [0x00];
    assemble Jmp = [0x01];
    assemble Ter = [0x02];
    assemble Cll = [0x03];
    assemble Ret = [0x04];
    assemble Mov = [0x10];
    assemble Cpy = [0x11];
    assemble Swp = [0x12];
    assemble Ieq = [0x20];
    assemble Inq = [0x21];
    assemble Igt = [0x22];
    assemble Ilt = [0x23];
    assemble Igq = [0x24];
    assemble Ilq = [0x25];
    assemble Neg = [0x30];
    assemble Or  = [0x31];
    assemble And = [0x32];
    assemble Xor = [0x33];
    assemble Rsh = [0x34];
    assemble Lsh = [0x35];
    assemble Spb = [0x36];
    assemble Add = [0x40];
    assemble Sub = [0x41];
    assemble Mul = [0x42];
    assemble Div = [0x43];
    assemble Mod = [0x44];
    assemble Adi = [0x50];
    assemble Ado = [0x51];

instance Assemble Direction where
    assemble Direct   = [0x0]
    assemble Indirect = [0x1]

instance Assemble ModeKind where
    assemble Memory           = [0x0];
    assemble Register         = [0x1];
    assemble RegisterPostIncr = [0x2];
    assemble RegisterPreIncr  = [0x3];
    assemble PcOffsetPositive = [0x4];
    assemble PcOffsetNegative = [0x5];
    assemble CarryFlag        = [0x6];

instance Assemble Mode where
    assemble (Mode kind indirect) = [((((head . assemble) indirect) `shift` 3)) .|.
                                       ((head . assemble) kind)];

instance Assemble Port where
    assemble (Port mode val) = [(head . assemble) mode] ++ (fromMaybe [] (sequence [val]))

instance Assemble Line where
    assemble (PlainData val) = val
    assemble (Instruction opcode portX portY) = [(((head . assemble) opcode) `shift` 8) .|.
                                                 (a `shift` 4) .|. b] ++ x ++ y
        where a:x = fromMaybe [0x0000] (fmap assemble portX)
              b:y = fromMaybe [0x0000] (fmap assemble portY)

instance ReadMaybe Opcode where
    readMaybe "nop" = Just Nop;
    readMaybe "jmp" = Just Jmp;
    readMaybe "ter" = Just Ter;
    readMaybe "cll" = Just Cll;
    readMaybe "ret" = Just Ret;
    readMaybe "mov" = Just Mov;
    readMaybe "cpy" = Just Cpy;
    readMaybe "swp" = Just Swp;
    readMaybe "ieq" = Just Ieq;
    readMaybe "inq" = Just Inq;
    readMaybe "igt" = Just Igt;
    readMaybe "ilt" = Just Ilt;
    readMaybe "igq" = Just Igq;
    readMaybe "ilq" = Just Ilq;
    readMaybe "neg" = Just Neg;
    readMaybe "or"  = Just Or;
    readMaybe "and" = Just And;
    readMaybe "xor" = Just Xor;
    readMaybe "rsh" = Just Rsh;
    readMaybe "lsh" = Just Lsh;
    readMaybe "spb" = Just Spb;
    readMaybe "add" = Just Add;
    readMaybe "sub" = Just Sub;
    readMaybe "mul" = Just Mul;
    readMaybe "div" = Just Div;
    readMaybe "mod" = Just Mod;
    readMaybe "adi" = Just Adi;
    readMaybe "ado" = Just Ado;
    readMaybe _     = Nothing;

instance ReadMaybe ModeKind where
    readMaybe "M" = Just Memory;
    readMaybe "R" = Just Register;
    readMaybe "A" = Just RegisterPostIncr;
    readMaybe "B" = Just RegisterPreIncr;
    readMaybe "P" = Just PcOffsetPositive;
    readMaybe "N" = Just PcOffsetNegative;
    readMaybe "C" = Just CarryFlag;
    readMaybe _   = Nothing;

instance ReadMaybe Direction where
    readMaybe ":" = Just Direct;
    readMaybe "@" = Just Indirect;
    readMaybe _   = Nothing;

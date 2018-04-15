module ReadMaybe where

import Syntax

class ReadMaybe a where
    readMaybe :: String -> Maybe a

instance ReadMaybe Opcode where
    readMaybe "nop" = Just Nop;
    readMaybe "jmp" = Just Jmp;
    readMaybe "ter" = Just Ter;
    readMaybe "cll" = Just Cll;
    readMaybe "ret" = Just Ret;
    readMaybe "psh" = Just Psh;
    readMaybe "pop" = Just Pop;
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
    readMaybe "aib" = Just Aib;
    readMaybe "aob" = Just Aob;
    readMaybe "aiw" = Just Aiw;
    readMaybe "aow" = Just Aow;
    readMaybe _     = Nothing;

instance ReadMaybe ModeKind where
    readMaybe "L" = Just Literal;
    readMaybe "R" = Just Register;
    readMaybe "A" = Just RegisterPostIncr;
    readMaybe "B" = Just RegisterPreIncr;
    readMaybe "P" = Just PcOffsetPositive;
    readMaybe "N" = Just PcOffsetNegative;
    readMaybe "C" = Just CarryFlag;
    readMaybe _   = Nothing;

instance ReadMaybe ModeDir where
    readMaybe ":" = Just Direct;
    readMaybe "&" = Just Indirect;
    readMaybe _   = Nothing;

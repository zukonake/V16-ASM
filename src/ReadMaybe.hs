module ReadMaybe where

import Syntax

class ReadMaybe a where
    readMaybe :: String -> Maybe a

instance ReadMaybe Opcode where
    readMaybe "nop"  = Just Nop
    readMaybe "jump" = Just Jump
    readMaybe "term" = Just Term
    readMaybe "call" = Just Call
    readMaybe "ret"  = Just Ret
    readMaybe "push" = Just Push
    readMaybe "pop"  = Just Pop
    readMaybe "move" = Just Move
    readMaybe "copy" = Just Copy
    readMaybe "swap" = Just Swap
    readMaybe "ifeq" = Just Ifeq
    readMaybe "ifnq" = Just Ifnq
    readMaybe "ifgt" = Just Ifgt
    readMaybe "iflt" = Just Iflt
    readMaybe "ifgq" = Just Ifgq
    readMaybe "iflq" = Just Iflq
    readMaybe "neg"  = Just Neg
    readMaybe "or"   = Just Or
    readMaybe "and"  = Just And
    readMaybe "xor"  = Just Xor
    readMaybe "rshf" = Just Rshf
    readMaybe "lshf" = Just Lshf
    readMaybe "swpb" = Just Swpb
    readMaybe "add"  = Just Add
    readMaybe "sub"  = Just Sub
    readMaybe "mul"  = Just Mul
    readMaybe "div"  = Just Div
    readMaybe "mod"  = Just Mod
    readMaybe "adbi" = Just Adbi
    readMaybe "adbo" = Just Adbo
    readMaybe "adwi" = Just Adwi
    readMaybe "adwo" = Just Adwo
    readMaybe _      = Nothing

instance ReadMaybe ModeKind where
    readMaybe "L" = Just Literal
    readMaybe "R" = Just Register
    readMaybe "A" = Just RegisterPostIncr
    readMaybe "B" = Just RegisterPreIncr
    readMaybe "P" = Just PcOffsetPositive
    readMaybe "N" = Just PcOffsetNegative
    readMaybe "C" = Just CarryFlag
    readMaybe _   = Nothing

instance ReadMaybe ModeDir where
    readMaybe ":" = Just Direct
    readMaybe "&" = Just Indirect
    readMaybe _   = Nothing

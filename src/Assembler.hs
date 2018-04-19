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

instance Assemble Word8 Word16 where
    assemble v = fromIntegral v

instance Assemble Mode Word16 where
    assemble (dir, kind) = assemble ((dir `shift` 3) .|. kind)

instance Assemble Port (Word16, Maybe Word16) where
    assemble (Port mode val) = ((assemble mode), val)

instance Assemble Node [Word16] where
    assemble (PlainData val) = val
    assemble (Instruction opcode portX portY) = do
        maybeApp y (maybeApp x [((assemble opcode `shift` 8) .|. (a `shift` 4) .|. b)])
            where (a, x) = maybe (0x0000, Nothing) assemble portX
                  (b, y) = maybe (0x0000, Nothing) assemble portY
                  maybeApp z zs = zs ++ (maybeToList z)


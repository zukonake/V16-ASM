module Syntax where

import Data.Word

type Opcode      = Word8;
type ModeKind    = Word8;
type ModeDir     = Word8;
type Instruction = Word8;

type Mode = (ModeDir, ModeKind)

data Port = Port Mode (Maybe Word16)

data Node = PlainData [Word16]
          | Instruction Opcode (Maybe Port) (Maybe Port)

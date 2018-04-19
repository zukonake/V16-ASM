{-# LANGUAGE ForeignFunctionInterface #-}
module Vortex where

import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Control.Monad.Except
import System.IO.Unsafe
import Data.Char
import Data.Word

import Error
import Syntax

foreign import ccall "&ERR_VAL" errorValue :: Ptr Word8
foreign import ccall "asm_op"   cAsmOp     :: CString -> Opcode
foreign import ccall "asm_mk"   cAsmMk     :: CString -> ModeKind

asmOp :: String -> ThrowsError Opcode
asmOp s = do
    let val = cstringCall (map toUpper s) cAsmOp
    if val == cConst errorValue
    then throwError $ IllegalOpcode s
    else return val

asmMk :: Char -> ThrowsError ModeKind
asmMk s = do
    let val = cstringCall [toUpper s] cAsmMk
    if val == cConst errorValue
    then throwError $ IllegalModeKind s
    else return val

cstringCall :: String -> (CString -> a) -> a
cstringCall s f = do
    let cstring = unsafePerformIO $ newCString s
    let val     = f cstring
    let _       = unsafePerformIO $ free cstring
    val

cConst :: Ptr Word8 -> Word8
cConst = (unsafePerformIO . peek)

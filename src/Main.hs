module Main where

import System.Environment
import System.IO
import Control.Monad.Except
import Data.Word
import Data.Bits
import qualified Data.ByteString as BS

import Text.Parsec.Prim

import Error
import Lexer
import Parser
import Assemble

encodeWords :: [Word16] -> BS.ByteString
encodeWords xs = (BS.pack . map fromIntegral . foldr (++) [] . map encode) xs
    where encode x = [x .&. 0xFF, (x .&. 0xFF00) `shiftR` 8]

assembleString :: BS.ByteString -> ThrowsError BS.ByteString
assembleString s = do
    case runParser lexString 0 "lexer" s of
        Left err  -> throwError $ ParsecError err
        Right val -> (readLines val) >>= (return . encodeWords . concat . map assemble)

main :: IO ()
main = do
    args <- getArgs
    if length args == 2
    then do
        input <- BS.readFile (args !! 0)
        case assembleString input of
            Left err  -> hPutStrLn stderr (show err)
            Right val -> BS.writeFile (args !! 1) val
    else if length args == 0
        then do
            input <- BS.getContents
            case assembleString input of
                Left err  -> hPutStrLn stderr (show err)
                Right val -> BS.putStr val
        else do
            programName <- getProgName
            putStrLn $ "Usage: " ++ programName ++ " INPUT_FILE OUTPUT_FILE"
            putStrLn "\tWhen no arguments are given, stdin and stdout will be used."

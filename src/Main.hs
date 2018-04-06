module Main where

import System.Environment
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

assembleString :: String -> ThrowsError BS.ByteString
assembleString s = do
    case runParser lexString 0 "lexer" s of
        Left err  -> throwError $ ParsecError err
        Right val -> (readLines val) >>= (return . encodeWords . concat . map assemble)

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2
    then do
        programName <- getProgName
        putStrLn $ "Usage: " ++ programName ++ " INPUT_FILE OUTPUT_FILE"
        return ()
    else do
        file <- readFile (args !! 0)
        case assembleString file of
            Left err  -> putStrLn $ show err
            Right val -> BS.writeFile (args !! 1) val

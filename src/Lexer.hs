module Lexer where

import Data.Word
import Data.Int
import Numeric

import Text.Parsec hiding (Line)

data Value = Literal  Word16
           | ConstRef String

data Port = Mode Char Char (Maybe Value)
          | LabelRef String

data Line = PlainData [Value]
          | Instruction String (Maybe Port) (Maybe Port)
          | ConstDef String Word16
          | LabelDef String Int16

type WordCount = Int

type Lexer = Parsec String WordCount

increaseWord :: Int -> Lexer ()
increaseWord n = modifyState (+ n)

lexString :: Lexer [Line]
lexString = do
    line `sepBy1` newline

line :: Lexer Line
line = do
    skipMany newline
    try constDef <|>
        try labelDef <|>
        try instruction <|>
        try plainData

constDef :: Lexer Line
constDef = do
    _     <- char '$'
    name  <- many1 letter
    skipMany1 separator
    val   <- unsigned
    return $ ConstDef name val

labelDef :: Lexer Line
labelDef = do
    _      <- char '.'
    name   <- many1 (letter <|> char '_')
    skipMany separator
    offset <- option 0 signed
    pos    <- getState
    return $ LabelDef name $ fromIntegral (pos + (fromIntegral offset) + 1)

instruction :: Lexer Line
instruction = do
    opcode <- count 3 letter
    increaseWord 1
    skipMany separator
    portA  <- optionMaybe port
    skipMany separator
    portB  <- optionMaybe port
    return $ Instruction opcode portA portB

plainData :: Lexer Line
plainData = do
    val <- value `sepBy` (many1 separator)
    increaseWord (length val)
    return $ PlainData val

port :: Lexer Port
port = try mode <|> try labelRef

mode :: Lexer Port
mode = do
    kind      <- letter
    direction <- oneOf ":@"
    val       <- optionMaybe value
    return $ Mode kind direction val

labelRef :: Lexer Port
labelRef = do
    _    <- char '.'
    name <- many1 (letter <|> char '_')
    increaseWord 1
    return $ LabelRef name

value :: Lexer Value
value = try constRef <|> try literal

constRef :: Lexer Value
constRef = do
    name <- many1 letter
    increaseWord 1
    return $ ConstRef name

literal :: Lexer Value
literal = do
    val <- unsigned
    increaseWord 1
    return $ Literal val

signed :: Lexer Int16
signed = do
    prefix <- optionMaybe $ oneOf "-+0"
    val    <- unsigned
    return $ if prefix == Just '-' then (negate . fromIntegral) val else fromIntegral val

unsigned :: Lexer Word16
unsigned = do
    optional $ string "0x"
    val <- many1 hexDigit
    (return . fst . head . readHex) val

separator :: Lexer Char
separator = oneOf "\t "

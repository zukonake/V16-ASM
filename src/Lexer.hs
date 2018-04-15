module Lexer where

import Data.Char (digitToInt)
import Data.Word (Word16)
import Data.Int  (Int16)
import Data.List (foldl')
import qualified Data.ByteString as BS
import Numeric

import Text.Parsec hiding (Line)

data Value = Literal  Word16
           | ConstRef String

data Port = Mode Char Char (Maybe Value)
          | LabelRef Char String

data Line = PlainData [Value]
          | Instruction String (Maybe Port) (Maybe Port)
          | ConstDef String Word16
          | LabelDef String Int16

type WordCount = Int

type Lexer = Parsec BS.ByteString WordCount

portSize :: Maybe Port -> Int
portSize (Just (Mode _ _ (Just _))) = 1
portSize (Just (LabelRef _ _))      = 1
portSize _                          = 0

increaseWord :: Line -> Lexer ()
increaseWord (PlainData xs)      = modifyState (+ (length xs))
increaseWord (Instruction _ x y) = modifyState (+ (1 + portSize x + portSize y))
increaseWord _                   = return ()

lexString :: Lexer [Line]
lexString = do
    line `endBy1` (skipMany1 newline <|> comment)

comment :: Lexer ()
comment = do
    _ <- optional separator
    _ <- char ';'
    _ <- manyTill anyChar newline
    return ()

line :: Lexer Line
line = do
    val <- try constDef <|>
           try labelDef <|>
           try instruction <|>
           try plainData
    increaseWord val
    return val

constDef :: Lexer Line
constDef = do
    _     <- string "@const"
    separator
    name  <- ident <?> "const identifier"
    separator
    val   <- unsigned <?> "const value"
    return $ ConstDef name val

labelDef :: Lexer Line
labelDef = do
    _      <- string "@label"
    separator
    name   <- ident <?> "label identifier"
    optional separator
    offset <- option 0 signed
    pos    <- getState
    return $ LabelDef name $ fromIntegral (pos + (fromIntegral offset) + 1)

instruction :: Lexer Line
instruction = do
    opcode <- ident <?> "opcode"
    optional separator
    portA  <- optionMaybe port
    optional separator
    portB  <- optionMaybe port
    return $ Instruction opcode portA portB

plainData :: Lexer Line
plainData = do
    val <- value `sepBy` (many1 separator)
    return $ PlainData val

port :: Lexer Port
port = try mode <|> try labelRef

mode :: Lexer Port
mode = do
    kind      <- option 'L' letter
    direction <- modeDir
    val       <- optionMaybe value
    return $ Mode kind direction val

labelRef :: Lexer Port
labelRef = do
    direction <- option ':' modeDir
    _         <- char '@'
    name      <- ident <?> "label reference identifier"
    return $ LabelRef direction name

modeDir :: Lexer Char
modeDir = oneOf ":&"

value :: Lexer Value
value = try constRef <|> try literal

constRef :: Lexer Value
constRef = do
    name <- ident <?> "const reference identifier"
    return $ ConstRef name

ident :: Lexer String
ident = do
    x  <- letter
    xs <- many (alphaNum <|> oneOf "_-")
    return (x:xs)

literal :: Lexer Value
literal = do
    val <- unsigned
    return $ Literal val

signed :: Lexer Int16
signed = do
    prefix <- optionMaybe $ oneOf "-+0"
    val    <- unsigned
    return $ if prefix == Just '-' then (negate . fromIntegral) val else fromIntegral val

unsigned :: Lexer Word16
unsigned = do
    optional $ char '0'
    base <- option 'd' (char 'x' <|> char 'd' <|> char 'o' <|> char 'b') <?> "numeric base"
    let (character, reader) = case base of
            'd' -> (digit,      fst . head . readDec)
            'x' -> (hexDigit,   fst . head . readHex)
            'o' -> (octDigit,   fst . head . readOct)
            'b' -> (oneOf "01", foldl' (\acc x -> acc * 2 + digitToInt x) 0)
            _   -> error "Invalid base parsed"
    val <- many1 character
    (return . fromIntegral . reader) val

separator :: Lexer ()
separator = skipMany1 $ oneOf "\t "

module Parser (readLines) where

import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Monad.State.Strict

import qualified Lexer as L
import Syntax
import Error
import ReadMaybe

type ConstDef = (String, Word16)
type LabelDef = (String, Int16)

data Env = Env {consts     :: [ConstDef],
                labels     :: [LabelDef],
                word_count :: Int}

defaultEnv :: Env
defaultEnv = Env {consts = [("ITEREG", 0x0000),
                            ("ARHREG", 0x0001),
                            ("OPEREG", 0x000D),
                            ("SP1REG", 0x000E),
                            ("SP2REG", 0x000F)],
                  labels = [],
                  word_count = 0x0000}

type Parser = ExceptT Error (State Env)

increaseWord :: Int -> Parser ()
increaseWord n = modify (\s -> s{word_count = (word_count s) + n})

addConstDef :: ConstDef -> Parser ()
addConstDef x = modify (\s -> s{consts = x:(consts s)})

addLabelDef :: LabelDef -> Parser ()
addLabelDef x = modify (\s -> s{labels = x:(labels s)})

readLines :: [L.Line] -> ThrowsError [Node]
readLines xs = do
    let (val, env) = runState (runExceptT (preprocessLines xs)) defaultEnv
    val >>= (\x -> evalState (runExceptT (parseLines x)) env)

preprocessLines :: [L.Line] -> Parser [L.Line]
preprocessLines [] = return []
preprocessLines ((L.ConstDef k v):xs) = do
    addConstDef (k, v)
    preprocessLines xs
preprocessLines ((L.LabelDef k v):xs) = do
    addLabelDef (k, v)
    preprocessLines xs
preprocessLines (x:xs) = fmap ((:) x) (preprocessLines xs)

parseLines :: [L.Line] -> Parser [Node]
parseLines [] = return []
parseLines ((L.PlainData val):xs) = do
    values <- (sequence . map parseValue) val
    increaseWord (length values)
    fmap ((:) (PlainData values)) (parseLines xs)
parseLines ((L.Instruction o a b):xs) = do
    opcode <- parseOpcode o
    portX  <- (sequence . fmap parsePort) a
    portY  <- (sequence . fmap parsePort) b
    fmap ((:) (Instruction opcode portX portY)) (parseLines xs)
parseLines ((L.ConstDef _ _):_) = error "Const definitions should be preprocessed first"
parseLines ((L.LabelDef _ _):_) = error "Label definitions should be preprocessed first"

parsePort :: L.Port -> Parser Port
parsePort (L.Mode k d v) = do
    kind      <- parseModeKind k
    direction <- parseModeDirection d
    val       <- (sequence . fmap parseValue) v
    return $ Port (Mode kind direction) val
parsePort (L.LabelRef dir name) = do
    s <- get
    case lookup name (labels s) of
        Just offset -> do
            let val  = (fromIntegral offset) - (word_count s)
            let kind = if val >= 0 then PcOffsetPositive else PcOffsetNegative
            increaseWord 1
            direction <- parseModeDirection dir
            return $ Port (Mode kind direction) (Just (fromIntegral (abs val)))
        Nothing -> throwError $ UndefinedLabel name

parseModeKind :: Char -> Parser ModeKind
parseModeKind x = do
    case readMaybe [x] of
        Just val -> return val
        Nothing  -> throwError $ IllegalModeKind [x]

parseModeDirection :: Char -> Parser ModeDir
parseModeDirection x = do
    case readMaybe [x] of
        Just val -> return val
        Nothing  -> throwError $ IllegalModeDirection [x]

parseOpcode :: String -> Parser Opcode
parseOpcode x = do
    case readMaybe x of
        Just val -> do
            increaseWord 1
            return val
        Nothing  -> throwError $ IllegalOpcode x

parseValue :: L.Value -> Parser Word16
parseValue (L.Literal val)   = do
    increaseWord 1
    return val
parseValue (L.ConstRef name) = do
    s <- get
    case lookup name (consts s) of
        Just val -> do
            increaseWord 1
            return val
        Nothing  -> throwError $ UndefinedConst name

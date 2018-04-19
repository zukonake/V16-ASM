module Parser (readLines) where

import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Monad.State.Strict

import qualified Lexer as L
import Syntax
import Error
import Vortex

type ConstDef = (String, Word16)
type LabelDef = (String, Int16)

data Env = Env {consts     :: [ConstDef],
                labels     :: [LabelDef],
                word_count :: Int}

defaultEnv :: Env
defaultEnv = Env {consts = [("SPR", 0xD),
                            ("SAR", 0xF),
                            ("SBR", 0xF)],
                  labels = [],
                  word_count = 0x0000}

type Parser = ExceptT Error (State Env)

portSize :: Maybe Port -> Int
portSize (Just (Port _ (Just _))) = 1
portSize _                 = 0

increaseWord :: Node -> Parser ()
increaseWord (PlainData xs) = modify (\s -> s{word_count = (word_count s) + (length xs)})
increaseWord (Instruction _ x y) = modify (\s -> s{word_count = (word_count s) +
                                           1 + portSize x + portSize y})

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
    let node = PlainData values
    increaseWord node
    fmap ((:) node) (parseLines xs)
parseLines ((L.Instruction o a b):xs) = do
    opcode <- parseOpcode o
    portX  <- (sequence . fmap parsePort) a
    portY  <- (sequence . fmap parsePort) b
    let node = Instruction opcode portX portY
    increaseWord node
    fmap ((:) node) (parseLines xs)
parseLines ((L.ConstDef _ _):_) = error "Const definitions should be preprocessed first"
parseLines ((L.LabelDef _ _):_) = error "Label definitions should be preprocessed first"

parsePort :: L.Port -> Parser Port
parsePort (L.Mode k d v) = do
    kind      <- parseModeKind k
    direction <- parseModeDirection d
    val       <- (sequence . fmap parseValue) v
    return $ Port (kind, direction) val
parsePort (L.LabelRef dir name) = do
    s <- get
    case lookup name (labels s) of
        Just offset -> do
            let val  = (fromIntegral offset) - (word_count s + 1) -- 1, because we count the opcode
            kind <- if val >= 0
                    then (ExceptT . return . asmMk) 'P'
                    else (ExceptT . return . asmMk) 'N'
            direction <- parseModeDirection dir
            return $ Port (kind, direction) (Just (fromIntegral (abs val)))
        Nothing -> throwError $ UndefinedLabel name

parseModeKind :: Char -> Parser ModeKind
parseModeKind x = (ExceptT . return . asmMk) x

parseModeDirection :: Char -> Parser ModeDir
parseModeDirection x = do
    case x of
        ':' -> return $ 0
        '&' -> return $ 1
        _   -> throwError $ IllegalModeDirection x

parseOpcode :: String -> Parser Opcode
parseOpcode x = (ExceptT . return . asmOp) x

parseValue :: L.Value -> Parser Word16
parseValue (L.Literal val)   = do
    return val
parseValue (L.ConstRef name) = do
    s <- get
    case lookup name (consts s) of
        Just val -> do
            return val
        Nothing  -> throwError $ UndefinedConst name

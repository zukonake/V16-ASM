module Error where

import Text.Parsec.Error

data Error = MultipleConstDefs    String
           | MultipleLabelDefs    String
           | UndefinedConst       String
           | UndefinedLabel       String
           | IllegalOpcode        String
           | IllegalModeKind      String
           | IllegalModeDirection String
           | ParsecError          ParseError
           | DefaultError         String

instance Show Error where
    show (MultipleConstDefs name) = "Multiple definitions of const " ++ name
    show (MultipleLabelDefs name) = "Multiple definitions of label " ++ name
    show (UndefinedConst    name) = "Undefined const reference " ++ name
    show (UndefinedLabel    name) = "Undefined label reference " ++ name
    show (IllegalOpcode     name) = "Illegal opcode " ++ name
    show (IllegalModeKind   name) = "Illegal mode kind " ++ name
    show (IllegalModeDirection d) = "Illegal mode direction " ++ d
    show (ParsecError message)    = "Parsec error at " ++ show message
    show (DefaultError message)   = show message

type ThrowsError = Either Error

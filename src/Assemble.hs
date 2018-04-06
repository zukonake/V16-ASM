module Assemble where

import Data.Word

class Assemble a where
    assemble :: a -> [Word16]

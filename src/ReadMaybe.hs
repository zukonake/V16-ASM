module ReadMaybe where

class ReadMaybe a where
    readMaybe :: String -> Maybe a

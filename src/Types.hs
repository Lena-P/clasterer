module Types (
  CSVDecodeOptions(..)
) where

data CSVDecodeOptions = 
    CSVDecodeOptions {delimiter :: Char,
                      hasHeader :: Bool,
                      ignoreFirstColumn :: Bool,
                      ignoreLastColumn :: Bool
               }
  deriving (Show, Read)
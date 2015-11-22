module Csv.CSVParser (
    parseCSVFile
) where

import           Data.Csv
import           Data.Char
import           Data.Vector
import qualified Data.ByteString.Lazy     as BL
import           Types

parseCSVFile :: String -> CSVDecodeOptions -> IO (Either String (Vector String))

parseCSVFile filePath decOpts = do
    csvData <- BL.readFile filePath
    let decoderOpts = DecodeOptions { decDelimiter = fromIntegral (ord $ delimiter decOpts) }
    let result = if hasHeader decOpts
                    then decodeWith decoderOpts HasHeader csvData
                    else decodeWith decoderOpts NoHeader csvData
    case result of Left err -> return (Left err)
                   Right v ->  do
                    return (Right (v :: Vector String) )
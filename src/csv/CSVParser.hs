module Csv.CSVParser (
    parseCSVFile
) where

import           Data.Csv
import           Data.Char
import           Data.Vector              as V
import qualified Data.ByteString.Lazy     as BL
import           Types

type Table a = V.Vector (V.Vector a)

parseCSVFile :: String -> CSVDecodeOptions -> IO (Either String (Table String))

parseCSVFile filePath decOpts = do
    csvData <- BL.readFile filePath
    let decoderOpts = DecodeOptions { decDelimiter = fromIntegral (ord $ delimiter decOpts) }
    let result = if hasHeader decOpts
                    then decodeWith decoderOpts HasHeader csvData
                    else decodeWith decoderOpts NoHeader csvData
    case result of Left err -> return (Left err)
                   Right v ->  do
                    return (Right (removeIgnoredColumns v decOpts) )

removeIgnoredColumns :: Table String -> CSVDecodeOptions -> Table String

removeIgnoredColumns csvData opts = removeFirstIfNeed (ignoreFirstColumn opts) $ removeLastIfNeed (ignoreLastColumn opts) csvData

removeLastIfNeed :: Bool -> Table String -> Table String

removeLastIfNeed need csvData = if need 
                                  then V.map V.init csvData
                                  else csvData

removeFirstIfNeed :: Bool -> Table String -> Table String

removeFirstIfNeed need csvData = if need 
                                    then V.map V.tail csvData
                                    else csvData
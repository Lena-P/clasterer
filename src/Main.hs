import System.Console.ArgParser
import Csv.CSVParser
import Types

data ComLineArgs =  -- First, we need a datatype
  ComLineArgs {inputFile :: String,
               outputFile :: String,
			   metric :: String,
			   count :: Int,
			   accuracy :: Float,
			   init :: String
			   }
  deriving (Show) -- we will print the values

argsParser -- Then, we define a parser 
  :: ParserSpec ComLineArgs
argsParser = ComLineArgs
  `parsedBy` reqPos "inFile"
  `andBy` optFlag "console" "outFile"
  `andBy` optFlag "euclid" "metric"
  `andBy` optFlag 3 "count"
  `andBy` optFlag 0.0001 "accuracy"
  `andBy` optFlag "matrix" "init"


process :: ComLineArgs -> IO ()
process args = do
  let opts = CSVDecodeOptions {delimiter = ',',
                      hasHeader = False,
                      ignoreFirstColumn = False,
                      ignoreLastColumn = True
               }
  parsedCsv <- parseCSVFile (inputFile args) opts
  case parsedCsv of
    Left s -> putStrLn s
    Right v -> print v
  return ()

main = do -- We proceed to build an interface and run it:
  interface <- mkApp argsParser
  runApp interface process
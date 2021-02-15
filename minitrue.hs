import CoC.DeBruijn
import CoC.Named
import CoC.Parser
import CoC.FileChecker
import Text.ParserCombinators.Parsec
import Data.Either
import System.Environment

main = do
  args <- getArgs
  m <- checkFile $ head args
  putStrLn $ maybe "Success" id m

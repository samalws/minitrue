import CoC.FileChecker
import System.Environment

main = do
  args <- getArgs
  m <- checkFile $ head args
  putStrLn $ maybe "Success" id m

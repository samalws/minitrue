import CoC.DeBruijn
import CoC.Named
import CoC.Parser
import CoC.FileChecker
import Text.ParserCombinators.Parsec
import Data.Either
import System.Environment

{-
tryParse a b = do
  pa <- parse termParser "term a" a
  pb <- parse termParser "term b" b
  return (pa, pb)

tryParseAndRun a b = text0 where
  text0 = either show text1 $ tryParse a b
  text1 (pa, pb) = maybe "Failed to convert a to de bruijn" (text2 pb) $ toDeBruijn [] pa
  text2 pb da = maybe "Failed to convert b to de bruijn" (text3 da) $ toDeBruijn [] pb
  text3 da db = if (hasType [] da db) then "a :: b" else "a !:: b"

mainRoutine = do
  putStrLn "enter a:"
  a <- getLine
  putStrLn "enter b:"
  b <- getLine
  putStrLn $ tryParseAndRun a b

main = mainRoutine >> main
-}

main = do
  args <- getArgs
  m <- checkFile $ head args
  putStrLn $ maybe "Success" id m
module CoC.FileChecker where

import CoC.Declaration
import CoC.Parser
import Text.ParserCombinators.Parsec

-- returns String if error, Nothing if no error
checkFile :: FilePath -> IO (Maybe String)
checkFile filePath = do
  text <- readFile filePath
  return $ either (Just . show) f $ parse codeParser filePath text where
    f :: Code -> Maybe String
    f = (either Just (const Nothing)) . checkCode 0 []

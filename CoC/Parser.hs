module CoC.Parser where

import CoC.Named
import CoC.DeBruijn
import Text.ParserCombinators.Parsec

varNameParser = many1 alphaNum

termParser :: GenParser Char st NTerm
termParser = starParser <|> piParser <|> lmParser <|> calledParser <|> varParser <|> parenTermParser
starParser = char '*' >> return NStar
piParser = do
  char 'π'
  spaces
  v <- varNameParser
  spaces
  char ':'
  spaces
  a <- termParser
  spaces
  char '.'
  spaces
  b <- termParser
  return $ NPi v a b
lmParser = do
  char 'λ'
  spaces
  v <- varNameParser
  spaces
  char ':'
  spaces
  a <- termParser
  spaces
  char '.'
  spaces
  b <- termParser
  return $ NLm v a b
calledParser = do
  char '`'
  spaces
  a <- termParser
  many1 space
  b <- termParser
  return $ NCalled a b
varParser = do
  v <- varNameParser
  return $ NVarTerm v
parenTermParser = do
  char '('
  spaces
  a <- termParser
  spaces
  char ')'
  return a

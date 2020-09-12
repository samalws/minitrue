module CoC.Parser where

import CoC.Named
import CoC.DeBruijn
import CoC.Declaration
import Text.ParserCombinators.Parsec
import Data.Char

-- <|> sucks!
infixr 1 <||>
a <||> b = try a <|> b

ignore :: GenParser Char st a -> GenParser Char st ()
ignore = (>> return ())
reservedChar c = elem c "()[]*π∀λ:;." || isSpace c
varNameParser = many1 $ satisfy $ not . reservedChar
withinComment = ignore $ do
  many $ noneOf "[]"
  optional (comment >> withinComment)
comment = ignore $ do
  char '['
  withinComment
  char ']'
whitespace = ignore $ many $ comment <||> ignore space

termParser :: GenParser Char st NTerm
termParser = starParser <||> piParser <||> lmParser <||> calledParser <||> varParser <||> parenTermParser
starParser = char '*' >> return NStar
piParser = do
  oneOf "π∀"
  whitespace
  v <- varNameParser
  whitespace
  string "::"
  whitespace
  a <- termParser
  whitespace
  char '.'
  whitespace
  b <- termParser
  return $ NPi v a b
lmParser = do
  char 'λ'
  whitespace
  v <- varNameParser
  whitespace
  char ':'
  whitespace
  a <- termParser
  whitespace
  char '.'
  whitespace
  b <- termParser
  return $ NLm v a b
calledParser = do
  a <- varParser <||> parenTermParser
  (space >> whitespace) <||> (whitespace >> ignore space)
  space
  whitespace
  b <- termParser
  return $ NCalled a b
varParser = do
  v <- varNameParser
  return $ NVarTerm v
parenTermParser = do
  char '('
  whitespace
  a <- termParser
  whitespace
  char ')'
  return a

declarationParser :: GenParser Char st Declaration
declarationParser = eqDeclarationParser <||> typeDeclarationParser
eqDeclarationParser = do
  v <- varNameParser
  whitespace
  char '='
  whitespace
  t <- termParser
  whitespace
  char ';'
  return $ EqDeclaration v t
typeDeclarationParser = do
  v <- varNameParser
  whitespace
  char ':'
  whitespace
  t <- termParser
  whitespace
  char ';'
  return $ TypeDeclaration v t

declarationParserAndWhitespace = do
  d <- declarationParser
  whitespace
  return d

codeParser :: GenParser Char st Code
codeParser = do
  whitespace
  code <- many declarationParserAndWhitespace
  eof
  return code

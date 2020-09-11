module CoC.Named where

import Data.List
import CoC.DeBruijn

-- n stands for named
type NVar = String
data NTerm = NStar | NPi NVar NTerm NTerm | NLm NVar NTerm NTerm | NCalled NTerm NTerm | NVarTerm NVar deriving (Read, Show)

toDeBruijn :: [NVar] -> NTerm -> Maybe Term
toDeBruijn e NStar = Just Star
toDeBruijn e (NPi v a b) = do
  da <- toDeBruijn e a
  db <- toDeBruijn (v:e) b
  return $ Pi da db
toDeBruijn e (NLm v a b) = do
  da <- toDeBruijn e a
  db <- toDeBruijn (v:e) b
  return $ Lm da db
toDeBruijn e (NCalled a b) = do
  da <- toDeBruijn e a
  db <- toDeBruijn e b
  return $ Called da db
toDeBruijn e (NVarTerm v) = do
  n <- elemIndex v e
  return $ VarTerm n

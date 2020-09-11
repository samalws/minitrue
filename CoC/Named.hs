module CoC.Named where

import CoC.DeBruijn
import Data.List
import Data.Maybe

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

nHasType :: NTerm -> NTerm -> Bool
nHasType a b = isJust $ do
  da <- toDeBruijn [] a
  db <- toDeBruijn [] b
  assert $ hasType [] da db

nValidTerm :: NTerm -> Bool
nValidTerm a = isJust $ do
  da <- toDeBruijn [] a
  assert $ validTerm [] da

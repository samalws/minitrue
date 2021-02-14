module CoC.Named where

import CoC.DeBruijn
import Data.List
import Data.Maybe

-- n stands for named
type NVar = String
data NTerm = NStar | NPi NVar NTerm NTerm | NLm NVar NTerm NTerm | NCalled NTerm NTerm | NVarTerm NVar deriving (Read, Show)

toDeBruijn :: [NVar] -> [(NVar, NTerm)] -> NTerm -> Maybe Term
toDeBruijn e e2 NStar = Just Star
toDeBruijn e e2 (NPi v a b) = do
  da <- toDeBruijn e e2 a
  db <- toDeBruijn (v:e) e2 b
  return $ Pi da db
toDeBruijn e e2 (NLm v a b) = do
  da <- toDeBruijn e e2 a
  db <- toDeBruijn (v:e) e2 b
  return $ Lm da db
toDeBruijn e e2 (NCalled a b) = do
  da <- toDeBruijn e e2 a
  db <- toDeBruijn e e2 b
  return $ Called da db
toDeBruijn e e2 (NVarTerm v) = maybe a (Just . VarTerm) b where
  a = lookup v e2 >>= toDeBruijn [] e2
  b = elemIndex v e

nTypeOf :: [(NVar, NTerm)] -> NTerm -> Maybe Term
nTypeOf e2 a = do
  da <- toDeBruijn [] e2 a
  typeOf [] da

nHasType :: [(NVar, NTerm)] -> NTerm -> NTerm -> Bool
nHasType e2 a b = isJust $ do
  da <- toDeBruijn [] e2 a
  db <- toDeBruijn [] e2 b
  assert $ hasType [] da db

nValidTerm :: [(NVar, NTerm)] -> NTerm -> Bool
nValidTerm e2 a = isJust $ do
  da <- toDeBruijn [] e2 a
  assert $ validTerm [] da

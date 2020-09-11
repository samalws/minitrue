module CoC.Declaration where

import CoC.Named

data Declaration = EqDeclaration NVar NTerm | TypeDeclaration NVar NTerm
type Code = [Declaration]
type CodeEnv = [(NVar, NTerm)]

err :: Int -> String -> Either String a
err n s = Left $ "Declaration " ++ show n ++ ": " ++ s

checkDeclaration :: Int -> CodeEnv -> Declaration -> Either String CodeEnv
checkDeclaration n e (EqDeclaration a b) = do
  maybe (err n $ "variable " ++ show a ++ " is taken") (const $ Right ()) (lookup a e)
  if (nValidTerm b) then (Right $ (a,b):e) else (err n "invalid term")
checkDeclaration n e (TypeDeclaration va b) = do
  a <- maybe (err n $ "variable " ++ show va ++ " is undefined") Right (lookup va e)
  if (nHasType a b) then (Right e) else (err n "type assertation failed")

checkCode :: Int -> CodeEnv -> Code -> Either String CodeEnv
checkCode _ e [] = Right e
checkCode n e (h:r) = do
  newE <- checkDeclaration n e h
  checkCode (n+1) newE r

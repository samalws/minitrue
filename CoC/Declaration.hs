module CoC.Declaration where

import CoC.Named

data Declaration = EqDeclaration NVar NTerm | TypeDeclaration NVar NTerm
type Code = [Declaration]
type CodeEnv = [(NVar, NTerm)]

err :: Int -> String -> Either String a
err n s = Left $ "Declaration " ++ show n ++ ": " ++ s

checkDeclaration :: Int -> CodeEnv -> Declaration -> Either String CodeEnv
checkDeclaration n e (EqDeclaration a b) = do
  maybe (Right ()) (const $ err n $ "variable " ++ show a ++ " is taken") (lookup a e)
  if (nValidTerm e b) then (Right $ (a,b):e) else (err n $ "invalid term: " ++ (show $ toDeBruijn [] e b))
checkDeclaration n e (TypeDeclaration va b) = do
  a <- maybe (err n $ "variable " ++ show va ++ " is undefined") Right (lookup va e)
  if (nHasType e a b) then (Right e) else (err n $ "type assertation failed; real type of " ++ (show $ toDeBruijn [] e a) ++ " is: " ++ (show $ nTypeOf e a))

checkCode :: Int -> CodeEnv -> Code -> Either String CodeEnv
checkCode _ e [] = Right e
checkCode n e (h:r) = do
  newE <- checkDeclaration n e h
  checkCode (n+1) newE r

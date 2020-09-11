import Data.Maybe

type Var = Int
data Term = Star | Pi Term Term | Lm Term Term | Called Term Term | VarTerm Var deriving (Read, Show)
type Env = [Term]

safeIndex :: Int -> [a] -> Maybe a
safeIndex _ [] = Nothing
safeIndex 0 (h:r) = Just h
safeIndex n (h:r) = safeIndex (n-1) r

assert :: Bool -> Maybe ()
assert True = Just ()
assert False = Nothing

incTerm :: Term -> Term
incTerm Star = Star
incTerm (Pi a b) = Pi (incTerm a) (incTerm b)
incTerm (Lm a b) = Lm (incTerm a) (incTerm b)
incTerm (Called a b) = Called (incTerm a) (incTerm b)
incTerm (VarTerm n) = VarTerm (n+1)

incEnv :: Env -> Env
incEnv = map incTerm

appendEnv :: Env -> Term -> Env
appendEnv e a = incEnv $ a:e

replace :: Var -> Term -> Term -> Term
replace _ _ Star = Star
replace v x (Pi a b) = Pi a $ replace (v+1) (incTerm x) b
replace v x (Lm a b) = Lm a $ replace (v+1) (incTerm x) b
replace v x (Called a b) = Called (replace v x a) (replace v x b)
replace v x (VarTerm a)
  | a == v = x
  | otherwise = VarTerm a

hasType :: Env -> Term -> Term -> Bool
hasType e a@(Pi b c) Star = validTerm e a
hasType e (Lm a b) p@(Pi c d) = validTerm e p && eqTerm e a c && hasType (appendEnv e a) b d
hasType e (Called a b) c = isJust $ do
  tb <- typeOf e b
  assert $ hasType e a (Pi tb c)
hasType e (VarTerm n) a = isJust $ do
  v <- safeIndex n e
  assert $ eqTerm e v a
hasType e a (Called b c) = isJust $ do
  d <- call e b c
  assert $ hasType e a d
hasType _ _ _ = False

typeOf :: Env -> Term -> Maybe Term
typeOf e Star = Nothing
typeOf e a@(Pi b c) = do
  assert $ validTerm e b
  assert $ validTerm (appendEnv e b) c
  return Star
typeOf e (Lm a b) = do
  assert $ validTerm e a
  tb <- typeOf (appendEnv e a) b
  return $ Pi a tb
typeOf e (Called a b) = do
  ta <- typeOf e a
  tb <- typeOf e b
  f ta tb where
    f (Pi c d) tb = do
      assert $ eqTerm e tb c
      return d
    f _ _ = Nothing
typeOf e (VarTerm n) = safeIndex n e

validTerm :: Env -> Term -> Bool
validTerm _ Star = True
validTerm e t = isJust $ typeOf e t

call :: Env -> Term -> Term -> Maybe Term
call e (Called a b) c = do
  d <- call e a b
  return $ Called d c
call e (Lm a b) c = do
  assert $ hasType e c a
  return $ replace 0 c b
call e _ _ = Nothing

eqTerm :: Env -> Term -> Term -> Bool
eqTerm e Star Star = True
eqTerm e (Pi a b) (Pi c d) = eqTerm e a c && eqTerm (appendEnv e a) b d
eqTerm e (Lm a b) (Lm c d) = eqTerm e a c && eqTerm (appendEnv e a) b d
eqTerm e x@(Called a b) y@(Called c d) = (eqTerm e a c && eqTerm e b d) || _eqTermHelper e x y || _eqTermHelper e y x
eqTerm e a@(Called _ _) b = _eqTermHelper e a b
eqTerm e a b@(Called _ _) = _eqTermHelper e b a
eqTerm e (VarTerm m) (VarTerm n) = m == n 
eqTerm e _ _ = False
_eqTermHelper e (Called a b) c = isJust $ do
  d <- call e a b
  assert $ eqTerm e d c

mainRoutine = do
  putStrLn "enter a:"
  a <- readLn
  putStrLn "enter b:"
  b <- readLn
  putStrLn $ if (hasType [] a b) then "a :: b" else "a !:: b"

main = mainRoutine >> main

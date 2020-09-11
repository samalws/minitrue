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

-- increment all variables in a term
incTerm :: Term -> Term
incTerm Star = Star
incTerm (Pi a b) = Pi (incTerm a) (incTerm b)
incTerm (Lm a b) = Lm (incTerm a) (incTerm b)
incTerm (Called a b) = Called (incTerm a) (incTerm b)
incTerm (VarTerm n) = VarTerm (n+1)

-- append a to e and increment all variables
appendEnv :: Env -> Term -> Env
appendEnv e a = map incTerm (a:e)

-- replace v with x in the term
replace :: Var -> Term -> Term -> Term
replace _ _ Star = Star
replace v x (Pi a b) = Pi a $ replace (v+1) (incTerm x) b
replace v x (Lm a b) = Lm a $ replace (v+1) (incTerm x) b
replace v x (Called a b) = Called (replace v x a) (replace v x b)
replace v x (VarTerm a)
  | a == v = x
  | otherwise = VarTerm a

-- call in environment e, checking type validity and returning result when possible
call :: Env -> Term -> Term -> Maybe Term
call e (Called a b) c = do
  d <- call e a b
  return $ Called d c
call e (Lm a b) c = do
  assert $ hasType e c a
  return $ replace 0 c b
call e _ _ = Nothing

-- if the term is validly typed in environment e, return its type
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

-- check if the term is valid
-- typeOf does this for us, so we can use that for everything except for Star (which has no type but is always valid)
validTerm :: Env -> Term -> Bool
validTerm _ Star = True
validTerm e t = isJust $ typeOf e t

-- check if a has type b in environment e, and that a and b are valid
hasType :: Env -> Term -> Term -> Bool
hasType e a b = isJust $ do
  ta <- typeOf e a
  assert $ eqTerm e ta b

-- check if 2 terms are both valid and are equal in environment e
eqTerm :: Env -> Term -> Term -> Bool
eqTerm e Star Star = True
eqTerm e (Pi a b) (Pi c d) = eqTerm e a c && eqTerm (appendEnv e a) b d
eqTerm e (Lm a b) (Lm c d) = eqTerm e a c && eqTerm (appendEnv e a) b d
eqTerm e (Called a b) (Called c d) = (eqTerm e a c && eqTerm e b d) || eqTermHelper e a b (Called c d) || eqTermHelper e c d (Called a b)
eqTerm e (Called a b) c = eqTermHelper e a b c
eqTerm e a (Called b c) = eqTermHelper e b c a
eqTerm e (VarTerm m) (VarTerm n) = m == n 
eqTerm e _ _ = False

eqTermHelper :: Env -> Term -> Term -> Term -> Bool
eqTermHelper e a b c = isJust $ do
  d <- call e a b
  assert $ eqTerm e d c

mainRoutine = do
  putStrLn "enter a:"
  a <- readLn
  putStrLn "enter b:"
  b <- readLn
  putStrLn $ if (hasType [] a b) then "a :: b" else "a !:: b"

main = mainRoutine >> main

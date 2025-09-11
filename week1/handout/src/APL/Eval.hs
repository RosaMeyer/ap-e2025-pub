module APL.Eval
  (
    Val(..),
    eval
  )
where

import APL.AST (Exp(..), VName)

data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

type Error = String

type Env = [(VName, Val)]

-- | Empty environment, which contains no variable bindings.
envEmpty :: Env
envEmpty = []

-- | Extend an environment with a new variable binding,
-- producing a new environment.
envExtend :: VName -> Val -> Env -> Env
envExtend vn val env = (vn, val) : env

-- | Look up a variable name in the provided environment.
-- Returns Nothing if the variable is not in the environment.
envLookup :: VName -> Env -> Maybe Val
envLookup _ [] = Nothing
envLookup vn env = lookup vn env


eval_helper env f exp1 exp2 = 
  case (eval env exp1, eval env exp2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right (ValInt (f x y))
    (Right _, Right _) -> Left "Non-integer operand"

eval :: Env -> Exp -> Either Error Val
eval _env (CstInt x) = Right (ValInt x)
eval _env (CstBool x) = Right (ValBool x)
eval env (Add x y) = eval_helper env (+) x y
eval env (Sub x y) = eval_helper env (-) x y
eval env (Mul x y) = eval_helper env (*) x y
eval env (Div _ (CstInt 0)) = Left ("Division by 0")
eval env (Div x y) = eval_helper env div x y 
eval env (Pow (CstInt x) (CstInt y)) 
  | y < 0  = Left ("Exponent negative")
  | y >= 0 = Right (ValInt (x ^ y))
eval env (Pow x y) = eval_helper env (^) x y
eval env (Eql x y) = 
  case (eval env x, eval env y) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right (ValBool (x == y))
    (Right (ValBool x), Right (ValBool y)) -> Right (ValBool (x == y))
    (Right _, Right _) -> Left "Type mismatch in equality"
eval env (If b x y) = 
  case (eval env b) of
    Left err -> Left err
    Right (ValInt _) -> Left "Non-boolean in if"
    Right (ValBool True) -> eval env x
    Right (ValBool False) -> eval env y 


{-
eval :: Exp -> Either Error Val
eval (CstInt x) = Right (ValInt x)
eval (CstBool x) = Right (ValBool x)
eval (Add x y) = eval_helper (+) x y
eval (Sub x y) = eval_helper (-) x y
eval (Mul x y) = eval_helper (*) x y
eval (Div _ (CstInt 0)) = Left ("Division by 0")
eval (Div x y) = eval_helper div x y 
eval (Pow (CstInt x) (CstInt y)) 
  | y < 0  = Left ("Exponent negative")
  | y >= 0 = Right (ValInt (x ^ y))
eval (Pow x y) = eval_helper (^) x y
eval (Eql x y) = 
  case (eval x, eval y) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right (ValBool (x == y))
    (Right (ValBool x), Right (ValBool y)) -> Right (ValBool (x == y))
    (Right _, Right _) -> Left "Type mismatch in equality"
eval (If b x y) = 
  case (eval b) of
    Left err -> Left err
    Right (ValInt _) -> Left "Non-boolean in if"
    Right (ValBool True) -> eval x
    Right (ValBool False) -> eval y -}



{- 
eval :: Exp -> Either Error Val
eval (CstInt x) = Right (ValInt x)
eval (Sub (CstInt x) (CstInt y)) = Right (ValInt (x - y))
eval (Add (CstInt x) (CstInt y)) = Right (ValInt (x + y))
eval (Mul (CstInt x) (CstInt y)) = Right (ValInt (x * y))
eval (Div (CstInt _) (CstInt 0)) = Left ("Division by 0")
eval (Div (CstInt x) (CstInt y)) = Right (ValInt (div x y))  --  (x `div`y)
eval (Pow (CstInt x) (CstInt y)) 
  | y < 0  = Left ("Exponent negative")
  | y >= 0 = Right (ValInt (x ^ y))
-}

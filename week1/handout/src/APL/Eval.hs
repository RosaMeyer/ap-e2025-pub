module APL.Eval
  (
    Val(..),
    eval
  )
where

import APL.AST (Exp(..))

data Val
  = ValInt Integer
  deriving (Eq, Show)

type Error = String

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
-- eval (Pow (CstInt x) (CstInt y)) = Right (ValInt (x ^ y))

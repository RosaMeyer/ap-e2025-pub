module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
    State,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type Error = String

newtype EvalM a = EvalM (Env -> State -> (State, Either Error a))

type Store = [(Val, Val)]
type State = ([String], Store)

stateEmpty :: State
stateEmpty = ([],[])

instance Functor EvalM where
  fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ \_ state -> (state, Right x)
  (<*>) = ap

-- -- TODO Check how they should handle the notion of failure
instance Monad EvalM where
  EvalM x >>= f = EvalM $ \env state ->
    let (state', val) = x env state
     in case val of
          (Left err) -> (state', Left err)
          (Right aval) ->
            ( let EvalM out = f aval
               in out env state'
            )

askEnv :: EvalM Env
askEnv = EvalM $ \env state -> (state, Right env)

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env state -> m (f env) state

failure :: String -> EvalM a
failure s = EvalM $ \_ state -> (state, Left s)

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env state ->
  case m1 env state of
    (state', Left _) -> m2 env state'
    (state', Right x) -> (state', Right x)

lookupStore :: Store -> Val -> Either Error Val
lookupStore ((key, val):rest) k = if k == key then Right val else lookupStore rest k 
lookupStore [] k = Left $ "Invalid key: " ++ show k

evalKvGet :: Val -> EvalM Val
evalKvGet k = EvalM $ \_env (pState, store) -> ((pState, store), lookupStore store k)

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut k v = EvalM $ \_env (pState, store) -> ((pState, (k, v) : store), Right ())

runEval :: EvalM a -> ([String], Either Error a)
runEval (EvalM m) =
  let ((state, _), d) = m envEmpty stateEmpty
   in (reverse state, d)

-- TODO do not think the right is correct
evalPrint :: String -> EvalM ()
evalPrint s = EvalM $ \_ (state, store) -> ((s : state, store), Right ())

evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f e1 e2 =
  evalIntBinOp f' e1 e2
  where
    f' x y = pure $ f x y

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp' (-) e1 e2
eval (Mul e1 e2) = evalIntBinOp' (*) e1 e2
eval (Div e1 e2) = evalIntBinOp checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y
eval (Pow e1 e2) = evalIntBinOp checkedPow e1 e2
  where
    checkedPow x y =
      if y < 0
        then failure "Negative exponent"
        else pure $ x ^ y
eval (Eql e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    (_, _) -> failure "Invalid operands to equality"
eval (If cond e1 e2) = do
  cond' <- eval cond
  case cond' of
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "Non-boolean conditional."
eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend var v1) $ eval e2
eval (ForLoop (loopparam, initial) (iv, bound) body) = do
  initial_v <- eval initial
  bound_v <- eval bound
  case bound_v of
    ValInt bound_int ->
      loop 0 bound_int initial_v
    _ ->
      failure "Non-integral loop bound"
  where
    loop i bound_int loop_v
      | i >= bound_int = pure loop_v
      | otherwise = do
          loop_v' <-
            localEnv (envExtend iv (ValInt i) . envExtend loopparam loop_v) $
              eval body
          loop (succ i) bound_int loop_v'
eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localEnv (const $ envExtend var arg f_env) $ eval body
    (_, _) ->
      failure "Cannot apply non-function"
eval (TryCatch e1 e2) =
  eval e1 `catch` eval e2
eval (Print s exp1) = do
  v <- eval exp1
  let string = case v of
        (ValInt x1) -> show x1
        (ValBool x1) -> show x1
        _ -> "#<fun>"
    in evalPrint $ s ++ ": " ++ string
  pure v
eval (KvPut k_exp v_exp) =
    eval k_exp >>= \k ->
    eval v_exp >>= \v ->
    case (k, v) of
        (x, y) -> evalKvPut x y >>=  \_ -> pure v
eval (KvGet k_exp) = 
    eval k_exp >>= \k -> evalKvGet k
    
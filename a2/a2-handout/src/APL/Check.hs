module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)

type Error = String

type Env = [(VName, ())] -- we only care about the names, not their types

newtype CheckM a = CheckM (Env -> (Either Error a, Env))

checkElem :: VName -> Env -> Bool
checkElem var env = case lookup var env of
  Just _  -> True
  Nothing -> False

-- If the variable is not in the scope, return an error. 
-- That way whenever a variable is hit we should stop there and not continue checking
checkVar :: VName -> CheckM ()
checkVar var = CheckM $ \env ->
  if checkElem var env
    then (Right (), env)
    else (Left $ "Variable not in scope: " ++ var, env)

-- If we hit a let expression we need to add the variable to the scope when checking the body of the let
checkLet :: VName -> Exp -> Exp -> CheckM ()
checkLet var e1 e2 = CheckM $ \env ->
    let (CheckM checkE1) = check e1
        (CheckM checkE2) = check e2
        (res1, _) = checkE1 env
        (res2, _) = checkE2 ((var, ()) : env)
    in case res1 of
        Left err -> (Left err, env)
        Right _ -> case res2 of
            Left err -> (Left err, env)
            Right _ -> (Right (), env)

-- If we hit a forloop expression we need to add the variable to the scope when checking the body of the forloop
checkForLoop :: (VName, Exp) -> (VName, Exp) -> Exp -> CheckM ()
checkForLoop (v1, e1) (v2, e2) body = CheckM $ \env ->
    let (CheckM checkE1) = check e1
        (res1, _) = checkE1 env
    in case res1 of
        Left err -> (Left err, env)
        Right _ ->
            let (CheckM checkE2) = check e2
                (res2, _) = checkE2 env
            in case res2 of
                Left err -> (Left err, env)
                Right _ ->
                    let (CheckM checkBody) = check body
                        (resBody, _) = checkBody ((v1, ()) : (v2, ()) : env)
                    in case resBody of
                        Left err -> (Left err, env)
                        Right _ -> (Right (), env)

-- If we hit a lambda expression we need to add the variable to the scope when checking the body of the lambda
checkLambda :: VName -> Exp -> CheckM ()
checkLambda var e1 = CheckM $ \env ->
    let (CheckM checkE) = check e1
        (res, _) = checkE ((var, ()) : env)
    in case res of
        Left err -> (Left err, env)
        Right _ -> (Right (), env)

check :: Exp -> CheckM ()
check (CstInt _) = CheckM $ \env -> (Right (), env)
check (CstBool _) = CheckM $ \env -> (Right (), env)
check (Var v) = checkVar v
check (Let v e1 e2) = checkLet v e1 e2
check (ForLoop (v1, e1) (v2, e2) body) = checkForLoop (v1, e1) (v2, e2) body
check (Lambda v e) = checkLambda v e

checkExp :: Exp -> Maybe Error
checkExp expr = case runCheckM (check expr) [] of
    (Left err, _) -> Just err
    (Right (), _) -> Nothing
    where
        runCheckM (CheckM f) env = f env   




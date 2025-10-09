module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty []
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _  _ (Pure x) = ([], Right x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s
    runEval' r _ (Free (StatePutOp s' k)) = runEval' r s' k
    runEval' r s (Free (PrintOp st a)) = 
      let (sl, ax) = runEval' r s a
        in (st:sl, ax)
    runEval' _ _ (Free (ErrorOp err)) = ([], Left err)
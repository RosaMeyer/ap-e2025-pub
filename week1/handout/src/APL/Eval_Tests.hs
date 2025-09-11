module APL.Eval_Tests (tests) where

import APL.AST (Exp(..))
import APL.Eval (Val(..), eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "eval" $ 
        eval (CstInt 5) 
        @?= (Right (ValInt 5)),
      
      testCase "Eql_int" $ 
        eval (Eql (CstInt 5) (CstInt 5))
        @?= (Right (ValBool True)),

      testCase "Eql_bool" $ 
        eval (Eql (CstBool True) (CstBool True))
        @?= (Right (ValBool True)),

      testCase "Add" $
        eval (Add (CstInt 5) (CstInt 2))
        @?= (Right (ValInt 7)),

      testCase "Sub" $
        eval (Sub (CstInt 5) (CstInt 2))
        @?= (Right (ValInt 3)),

      testCase "Mul" $
        eval (Mul (CstInt 5) (CstInt 2))
        @?= (Right (ValInt 10)),

      testCase "Div_by_0" $
        eval (Div (CstInt 6) (CstInt 0))
        @?= (Left "Division by 0"),

      testCase "Div" $
        eval (Div (CstInt 6) (CstInt 2))
        @?= (Right (ValInt 3)),

      testCase "Pow_by_0" $
        eval (Pow (CstInt 5) (CstInt (-2)))
        @?= (Left "Exponent negative"),

      testCase "Pow" $
        eval (Pow (CstInt 5) (CstInt 2))
        @?= (Right (ValInt 25)),

      testCase "If_non_bool" $
        eval (If (CstInt 5) (CstInt 2) (CstInt 3))
        @?= (Left "Non-boolean in if"),

      testCase "If_true" $
        eval (If (CstBool True) (CstInt 2) (CstInt 3))
        @?= (Right (ValInt 2)),
        
      testCase "If_false" $
        eval (If (CstBool False) (CstInt 2) (CstInt 3))
        @?= (Right (ValInt 3))
    
    ]

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
        @?= (Right (ValInt 25))
    ]

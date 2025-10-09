module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Error, Val (..), eval, runEval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalTests :: TestTree
evalTests =
  testGroup
    "EValuation"
    [ testCase "Add" $
        eval' (Add (CstInt 2) (CstInt 5))
          @?= ([], Right (ValInt 7)),
      --
      testCase "Add (wrong type)" $
        eval' (Add (CstInt 2) (CstBool True))
          @?= ([], Left "Non-integer operand"),
      --
      testCase "Sub" $
        eval' (Sub (CstInt 2) (CstInt 5))
          @?= ([], Right (ValInt (-3))),
      --
      testCase "Div" $
        eval' (Div (CstInt 7) (CstInt 3))
          @?= ([], Right (ValInt 2)),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero"),
      --
      testCase "Pow" $
        eval' (Pow (CstInt 2) (CstInt 3))
          @?= ([], Right (ValInt 8)),
      --
      testCase "Pow0" $
        eval' (Pow (CstInt 2) (CstInt 0))
          @?= ([], Right (ValInt 1)),
      --
      testCase "Pow negative" $
        eval' (Pow (CstInt 2) (CstInt (-1)))
          @?= ([], Left "Negative exponent"),
      --
      testCase "Eql (false)" $
        eval' (Eql (CstInt 2) (CstInt 3))
          @?= ([], Right (ValBool False)),
      --
      testCase "Eql (true)" $
        eval' (Eql (CstInt 2) (CstInt 2))
          @?= ([], Right (ValBool True)),
      --
      testCase "If" $
        eval' (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= ([], Right (ValInt 2)),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5)),
      --
      testCase "ForLoop" $
        eval'
          (ForLoop ("p", CstInt 0) ("i", CstInt 10) (Add (Var "p") (Var "i")))
          @?= ([], Right (ValInt 45)),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right (ValBool True)),
      --
      testCase "Lambda/Apply" $
        eval'
          (Apply (Lambda "x" (Mul (Var "x") (Var "x"))) (CstInt 4))
          @?= ([], Right (ValInt 16)),
      --
      testCase "TryCatch" $
        eval'
          (TryCatch (Div (CstInt 7) (CstInt 0)) (CstBool True))
          @?= ([], Right (ValBool True))
    ]

printTests :: TestTree
printTests =
  testGroup
    "Task 1: Printing"
    [ testCase "Assignment Example 1" $
        eval' (Print "foo" $ CstInt 2)
          @?= (["foo: 2"], Right (ValInt 2)),
      testCase "Assignment Example 2" $
        eval' (Let "x" (Print "foo" $ CstInt 2) (Print "bar" $ CstInt 3))
          @?= (["foo: 2", "bar: 3"], Right (ValInt 3)),
      testCase "Assignment Example 3" $
        eval' (Let "x" (Print "foo" $ CstInt 2) (Var "bar"))
          @?= (["foo: 2"], Left "Unknown variable: bar"),
      testCase "Simple Int case" $
        eval' (Print "Int value" (CstInt 2))
          @?= (["Int value: 2"], Right (ValInt 2)),
      testCase "Simple Bool case" $
        eval' (Print "Bool value" (CstBool True))
          @?= (["Bool value: True"], Right (ValBool True)),
      testCase "Simple Fun case" $
        eval' (Print "Fun value" (Lambda "x" (Add (Var "x") (CstInt 5))))
          @?= (["Fun value: #<fun>"], Right (ValFun [] "x" (Add (Var "x") (CstInt 5)))),
      testCase "Simple Failure case" $
        eval' (Print "Failure value" (Div (CstBool True) (CstInt 5)))
          @?= ([], Left "Non-integer operand"),
      testCase "Simple Failure case" $
        eval' (Print "Failure value" (Div (CstBool True) (CstInt 5)))
          @?= ([], Left "Non-integer operand"),
      testCase "Print In For Loop" $
        eval' (Print "Last Print" (ForLoop ("p", CstInt 0) ("i", CstInt 5) (Print "loop" (Var "i"))))
          @?= ( ["loop: 0", "loop: 1", "loop: 2", "loop: 3", "loop: 4", "Last Print: 4"],
                Right (ValInt 4)
              ),
      testCase "Print Order" $
        eval'
          ( Print
              "6"
              ( Print
                  "5"
                  ( Apply
                      ( Print
                          "1"
                          (Lambda "x" (Print "4" (Add (Var "x") (Var "x"))))
                      )
                      (Print "3" (Print "2" (CstInt 1)))
                  )
              )
          )
          @?= (["1: #<fun>", "2: 1", "3: 1", "4: 2", "5: 2", "6: 2"], Right (ValInt 2)),
      testCase "Print Order (Failure of the first)" $
        eval'
          ( Print
              "6"
              ( Print
                  "5"
                  ( Apply
                      ( Print
                          "1"
                          (Lambda "x" (Print "4" (Add (Var "x") (Var "x"))))
                      )
                      (Print "3" (Print "2" (Pow (CstInt 1) (CstInt (-5)))))
                  )
              )
          )
          @?= (["1: #<fun>"], Left "Negative exponent"),
      testCase "Print Order (Failure of the last)" $
        eval'
          ( Print
              "5"
              ( Apply
                  ( Print
                      "1"
                      (Lambda "x" (Print "4" (Div (Var "x") (Var "x"))))
                  )
                  (Print "3" (Print "2" (Add (CstInt 1) (CstInt (-1)))))
              )
          )
          @?= (["1: #<fun>", "2: 0", "3: 0"], Left "Division by zero"),
      testCase "No string" $
        eval' (Print "" (Div (CstInt 1) (CstInt 5)))
          @?= ([": 0"], Right (ValInt 0))
    ]

kvTests :: TestTree
kvTests =
  testGroup
    "Task 2: Key-value store"
    [
        testCase "Assignment Example 1 (Simple KvPut & KvGet)" $
            eval'   (Let "x" (KvPut (CstInt 0) (CstBool True))
                    (KvGet (CstInt 0)))
            @?= ([],Right (ValBool True)),

        testCase "Assignment Example 2 (Non Existent Key)" $
            eval'   (Let "x" (KvPut (CstInt 0) (CstBool True))
                    (KvGet (CstInt 1)))
            @?= ([],Left "Invalid key: ValInt 1"),

        testCase "Assignment Example 3 (Shadowing Example)" $
            eval'   (Let "x" (KvPut (CstInt 0) (CstBool True))
                    (Let "y" (KvPut (CstInt 0) (CstBool False))
                    (KvGet (CstInt 0))))
            @?= ([],Right (ValBool False)),

        testCase "Key Failure" $
            eval'   (Let "x" (KvPut (Div (CstInt 0) (CstInt 0)) (CstBool True))
                    (KvGet (CstInt 0)))
            @?= ([], Left "Division by zero"),

        testCase "Val Failure" $
            eval'   (Let "x" (KvPut (CstInt 0) (Div (CstInt 0) (CstInt 0)))
                    (KvGet (CstInt 0)))
            @?= ([], Left "Division by zero"),

        testCase "Key-Value Shadowing" $
            eval'   (Let "x" (KvPut (CstInt 0) (CstBool True)) (Let "x" (KvGet (CstInt 0)) (KvGet (CstInt 0))))
            @?= ([], Right (ValBool True))
    ]

tests :: TestTree
tests = testGroup "Evaluation" [evalTests, printTests, kvTests]

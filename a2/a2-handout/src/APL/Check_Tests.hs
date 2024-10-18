{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module APL.Check_Tests (tests) where

import APL.AST (Exp (..))
import APL.Check (runCheck, checkExp, String, CheckM)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Data.Maybe (Maybe (Nothing))

checkExp' :: Exp -> Maybe String
checkExp' exp = runCheck (checkExp exp)

-- Assert that the provided expression should pass the type checker.
testPos :: Exp -> TestTree
testPos e =
  testCase (show e) $
    checkExp' e @?= Nothing

-- Assert that the provided expression should fail the type checker.
testNeg :: Exp -> TestTree
testNeg e =
  testCase (show e) $
    case checkExp' e of
      Nothing -> assertFailure "expected error"
      Just _ -> pure ()

tests :: TestTree
tests =
  testGroup
    "Checking"
    [testPos (CstInt 2), 
    testPos (CstBool False), 
    testNeg (Var "x"), 
    testPos (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x")), 
    testNeg (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "y")),
    testNeg (Let "x" (Add (Let "y" (CstInt 2) (Var "y")) (CstInt 3)) (Var "y")), -- This tests that the inner scope does not affect the outer scope
    testPos (Let "x" (Lambda "y" (Var "x")) (CstInt 2)),
    testPos (Let "x" (CstInt 10) (Let "y" (Add (Var "x") (CstInt 5)) (Add (Var "x") (Var "y")))),
    testPos (Lambda "x" (Var "x"))
    ]

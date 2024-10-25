module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import APL.Monad (Val(ValInt), EvalOp (KvGetOp, TransactionOp, KvPutOp), failure, evalKvPut)

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests]

put0 :: Val -> Val -> EvalM Val -> EvalM Val
put0 key val m = Free $ KvPutOp key val m

get0 :: Val -> EvalM Val
get0 key = Free $ KvGetOp key $ \val -> pure val

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testCase "localEnv" $
        runEval
          ( localEnv (const [("x", ValInt 1)]) $
              askEnv
          )
          @?= ([], Right [("x", ValInt 1)]),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5)),
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
      testCase "State" $
        runEval
          ( do
              putState [(ValInt 0, ValInt 1)]
              modifyState $ map (\(key, _) -> (key, ValInt 5))
              getState
          )
          @?= ([], Right [(ValInt 0, ValInt 5)]),
      --
      testCase "State" $
        runEval
          ( do
              putState [(ValInt 0, ValInt 1)]
              modifyState $ map (\(key, _) -> (key, ValInt 5))
              getState
          )
          @?= ([], Right [(ValInt 0, ValInt 5)]),
      testCase "Print" $
        runEval (evalPrint "test")
          @?= (["test"], Right ()),
      --
      testCase "Error" $
        runEval
          ( do
              _ <- failure "Oh no!"
              evalPrint "test"
          )
          @?= ([], Left "Oh no!"),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero"),
      --
      testCase "TryCatch" $
        runEval (Free (TryCatchOp (failure "Oh no!") (pure "Success!"))) @?= ([], Right "Success!"),
      --
      testCase "TryCatchDivByZero" $
        eval' (TryCatch (CstInt 5) (CstInt 1 `Div` CstInt 0)) @?= ([], Right (ValInt 5)),
      --
      testCase "put0 and get0" $
        runEval (put0 (ValInt 0) (ValInt 1) (get0 (ValInt 0))) @?= ([], Right (ValInt 1)),
      --
      testCase "put put and get" $
        runEval (put0 (ValInt 0) (ValInt 1) (put0 (ValInt 0) (ValInt 5) (get0 (ValInt 0)))) @?= ([], Right (ValInt 5)),
      --
      testCase "put0 and get not found" $
        runEval (put0 (ValInt 0) (ValInt 1) (get0 (ValInt 1))) @?= ([], Left "Key not found"),
      --
      testCase "evalKvPut and Get" $
        runEval
          ( do
              evalKvPut (ValInt 0) (ValInt 1)
              evalKvGet (ValInt 0)
          )
          @?= ([], Right (ValInt 1)),
      --
      testCase "transaction 1" $
        runEval (Free (TransactionOp
          (Free (KvPutOp (ValInt 0) (ValInt 1) (pure ())))
          (Free (KvGetOp (ValInt 0) pure))))
        @?= ([], Right (ValInt 1)),
      --
      testCase "transaction print failure" $
        runEval (Free (TransactionOp
        (Free (PrintOp "Test" (Free (KvPutOp (ValInt 1) (ValInt 0) (Free (KvGetOp (ValInt 0) (\_ -> pure ())))))))
        (pure "Success!"))) @?= (["Test"], Right "Success!"),
      --
      testCase "transaction print failure check if state has been updated in the transaction" $
        runEval (Free (TransactionOp
        (Free (PrintOp "Test" (Free (KvPutOp (ValInt 1) (ValInt 0) (Free (KvGetOp (ValInt 0) (\_ -> pure ())))))))
        (Free (KvGetOp (ValInt 1) pure)))) @?= (["Test"], Left "Key not found")
    ]

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    [ testCase "print" $ do
        let s1 = "Lalalalala"
            s2 = "Weeeeeeeee"
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalPrint s1
              evalPrint s2
        (out, res) @?= ([s1, s2], Right ()),

      --
      testCase "TryCatchIO" $ do
        let badEql = CstInt 0 `Eql` CstBool True
        result <- evalIO' (TryCatch badEql (CstInt 1 `Div` CstInt 0))
        result @?= Left ("Division by zero" :: Error),

      --
      testCase "IO StatePutOp and StateGetOp" $ do
        result <- runEvalIO (do
          Free (StatePutOp [(ValInt 1, ValInt 100), (ValBool True, ValBool False)] (Free (StateGetOp pure))))
        result @?= Right [(ValInt 1, ValInt 100), (ValBool True, ValBool False)],
      --
      testCase "IO KvPutOp and KvGetOp" $ do
        result <- runEvalIO (do
          Free (KvPutOp (ValInt 1) (ValInt 0) (Free (KvPutOp (ValInt 2) (ValInt 10) (Free (KvGetOp (ValInt 1) pure))))))
        result @?= Right (ValInt 0),
      --
      testCase "IO KvPutOp and KvGetOp 2" $ do
        result <- runEvalIO (do
          Free (KvPutOp (ValInt 1) (ValInt 0) (Free (KvPutOp (ValInt 2) (ValInt 10) (Free (KvGetOp (ValInt 2) pure))))))
        result @?= Right (ValInt 10),
      --
      testCase "IO Missing Key Test" $ do
        (_, res) <-
          captureIO ["ValInt 0"] $
            runEvalIO $
              Free $ KvPutOp (ValInt 0) (ValInt 1) (Free (KvGetOp (ValInt 1) pure))
        res @?= Right (ValInt 1),

      --
      testCase "IO Transaction" $ do
        result <- runEvalIO (do
          Free (TransactionOp
            (Free (KvPutOp (ValInt 2) (ValInt 10) (pure ())))
            (Free (KvGetOp (ValInt 2) pure)))
          )
        result @?= Right (ValInt 10),
      
      testCase "IO Fail Transaction" $ do
        (_, res) <-
          captureIO ["ValInt 1"] $
            runEvalIO $
              Free (TransactionOp
                (Free (KvPutOp (ValInt 2) (ValInt 10) (failure "oh shit")))
                (Free (KvPutOp (ValInt 1) (ValInt 0) (Free (KvGetOp (ValInt 2) pure)))))
        res @?= Right (ValInt 0),

        testCase "IO Fail Nested Transaction" $ do
        (_, res) <-
          captureIO ["ValInt 1"] $
            runEvalIO $
              Free (TransactionOp
                (Free (KvPutOp (ValInt 2) (ValInt 10) 
                  (Free (TransactionOp (evalKvPut (ValInt 3) (ValInt 4) >> failure "die") (pure ())))))
                (Free (KvPutOp (ValInt 1) (ValInt 0) (Free (KvGetOp (ValInt 2) pure)))))
        res @?= Right (ValInt 10),

        testCase "IO Fail Nested Transaction 2" $ do
        (_, res) <-
          captureIO ["ValInt 2"] $
            runEvalIO $
              Free (TransactionOp
                (Free (KvPutOp (ValInt 2) (ValInt 10) 
                  (Free (TransactionOp (evalKvPut (ValInt 3) (ValInt 4) >> failure "die") (pure ())))))
                (Free (KvPutOp (ValInt 1) (ValInt 0) (Free (KvGetOp (ValInt 3) pure)))))
        res @?= Right (ValInt 10)






        -- NOTE: This test will give a runtime error unless you replace the
        -- version of `eval` in `APL.Eval` with a complete version that supports
        -- `Print`-expressions. Uncomment at your own risk.
        -- testCase "print 2" $ do
        --    (out, res) <-
        --      captureIO [] $
        --        evalIO' $
        --          Print "This is also 1" $
        --            Print "This is 1" $
        --              CstInt 1
        --    (out, res) @?= (["This is 1: 1", "This is also 1: 1"], Right $ ValInt 1)
    ]

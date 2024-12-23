module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _ _ (Pure x) = ([], pure x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s
    runEval' r _ (Free (StatePutOp s' m)) = runEval' r s' m
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res) = runEval' r s m
       in (p : ps, res)
    runEval' r s (Free (TryCatchOp tryOp catchOp)) = runEval' r s $ catch tryOp catchOp
    runEval' r s (Free (KvGetOp key k)) =
      case lookup key s of
        Just val -> runEval' r s $ k val
        Nothing -> ([], Left "Key not found")
    runEval' r s (Free (KvPutOp key val m)) =
      let newState = (key, val) : filter ((/= key) . fst) s
      in runEval' r newState m
    runEval' r s (Free (TransactionOp a m)) =
      let a' = runEval' r s a in
        case a' of
          (s', Right ()) -> do
            state <- runEval' r s (a >> getState)
            case state of
              Right newState ->
                let (ps, res) = runEval' r newState m 
                in (s' ++ ps, res)
              Left e -> ([], Left e)
          (s', Left _) -> let (ps, res) = runEval' r s m 
                          in (s' ++ ps, res)


    runEval' _ _ (Free (ErrorOp e)) = ([], Left e)

module APL.Eval
  ( Val (..),
    eval,
    runEval,
    envEmpty,
    Error,
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

newtype EvalM a = EvalM (Env -> Either Error a)

{-Before Env

instance Functor EvalM where
  fmap _ (EvalM (Left e)) = EvalM $ Left e
  fmap f (EvalM (Right x)) = EvalM $ Right $ f x

instance Applicative EvalM where
  pure x = EvalM $ Right x
  EvalM (Left e)  <*> _               = EvalM $ Left e
  _               <*> EvalM (Left e)  = EvalM $ Left e
  EvalM (Right f) <*> EvalM (Right x) = EvalM $ Right $ f x 

instance Monad EvalM where
  EvalM x >>= f = EvalM $ case x of
    Left e -> Left e
    Right x' -> let EvalM y = f x' in y -}

instance Functor EvalM where
  fmap f (EvalM x) = EvalM $ \env -> case x env of
    Right v -> Right $ f v
    Left err -> Left err

instance Applicative EvalM where
  pure x = EvalM $ \env -> Right x
  EvalM ef <*> EvalM ex = EvalM $ \env ->
    case (ef env, ex env) of
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (Right f, Right x) -> Right (f x)

instance Monad EvalM where
  EvalM x >>= f = EvalM $ \env ->
    case x env of
      Left err -> Left err
      Right x' ->
        let EvalM y = f x' in y env


runEval :: EvalM a -> Either Error a
runEval (EvalM x) = x envEmpty

askEnv :: EvalM Env
askEnv = EvalM $ \env -> Right env

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env -> m (f env)

failure :: String -> EvalM a
failure s = EvalM $ \_env -> Left s

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env ->
  case m1 env of
    Left _ -> m2 env
    Right x -> Right x


helperOperator :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
helperOperator f e1 e2 = do
    x <- eval e1
    y <- eval e2
    case (x, y) of
      (ValInt x', ValInt y') -> ValInt <$> f x' y'
      _ -> failure "Non-integer operand"

helperOperator' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
helperOperator' f e1 e2 =
  helperOperator f' e1 e2
  where
    f' x y = pure $ f x y

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool x) = pure $ ValBool x
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend var v1) $ eval e2
eval (If cond e1 e2) = do
  cond' <- eval cond
  case cond' of
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "Non-boolean conditional."
eval (Add e1 e2) = do
  x <- eval e1
  y <- eval e2
  case (x, y) of
    (ValInt x', ValInt y') -> pure $ ValInt $ x' + y'
    _ -> failure "Non-integer operand"
eval (Sub e1 e2) = helperOperator' (-) e1 e2
eval (Mul e1 e2) = helperOperator' (*) e1 e2
eval (Div e1 e2) = helperOperator checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y
eval (Pow e1 e2) = helperOperator checkedNegExponent e1 e2
  where
    checkedNegExponent x y = 
      if y < 0
        then failure "Negative Exponent"
        else pure $ x ^ y
eval (Eql e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    _ -> failure "Invalid operands to equality"
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

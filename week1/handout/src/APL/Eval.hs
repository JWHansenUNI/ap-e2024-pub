module APL.Eval
  (
    eval, Val(..), envEmpty, envExtend, envLookup
  )
where
import APL.AST (Exp (..), VName)

data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

-- Naive Implementation

{-eval :: Exp -> Val
eval (CstInt x) = ValInt x
eval (Add exp1 exp2) = 
  let ValInt v1 = eval exp1
      ValInt v2 = eval exp2
  in ValInt (v1 + v2)
eval (Sub exp1 exp2) =
  let ValInt v1 = eval exp1
      ValInt v2 = eval exp2
  in ValInt (v1 - v2)
eval (Mul exp1 exp2) =
  let ValInt v1 = eval exp1
      ValInt v2 = eval exp2
  in ValInt (v1 * v2)
eval (Div exp1 exp2) =
  let ValInt v1 = eval exp1
      ValInt v2 = eval exp2
  in ValInt (v1 `div` v2)
eval (Pow exp1 exp2) =
  let ValInt v1 = eval exp1
      ValInt v2 = eval exp2
  in ValInt (v1 ^ v2)-}

  
type Error = String

type Env = [(VName, Val)]

helperOperator :: (Integer -> Integer -> Either Error Integer) -> Env -> Exp -> Exp -> Either Error Val
helperOperator f env e1 e2  =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> 
      case f x y of
        Left err -> Left err
        Right z -> Right $ ValInt z
    (_, _) -> Left "Non-integer operand"

helperOperator' :: (Integer -> Integer -> Integer) -> Env -> Exp -> Exp -> Either Error Val
helperOperator' f env e1 e2 =
  helperOperator f' env e1 e2
  where
    f' x y = Right $ f x y

eval :: Env -> Exp -> Either Error Val
eval env (CstInt x) = Right $ ValInt x
eval env (CstBool x) = Right $ ValBool x
eval env (Eql e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValBool $ x == y
    (Right (ValBool x), Right (ValBool y)) -> Right $ ValBool $ x == y
    (_, _) -> Left "Invalid operands to equality"
eval env (If cond e1 e2) =
  case eval env cond of
    Left err -> Left err
    Right (ValBool True) -> eval env e1
    Right (ValBool False) -> eval env e2
    Right _ -> Left "Non-boolean conditional"

eval env (Add e1 e2) = helperOperator' (+) env e1 e2
eval env (Mul e1 e2) = helperOperator' (*) env e1 e2
eval env (Sub e1 e2) = helperOperator' (-) env e1 e2
eval env (Div e1 e2) =
  helperOperator checkedDiv env e1 e2
    where
      checkedDiv _ 0 = Left "Division by zero!"
      checkedDiv x y = Right $ x `div` y
eval env (Pow e1 e2) =
  helperOperator checkedNegExponent env e1 e2
    where
      checkedNegExponent x y =
        if y < 0
          then Left "Negative exponent!"
          else Right $ x ^ y
eval env (Var vname) =
  case envLookup vname env of
    Just x -> Right x
    Nothing -> Left $ "Unknown variable: " ++ vname
eval env (Let var e1 e2) =
  case eval env e1 of
    Left err -> Left err
    Right v -> eval (envExtend var v env) e2

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env


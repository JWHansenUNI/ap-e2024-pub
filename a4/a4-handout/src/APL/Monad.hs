module APL.Monad
  ( envEmpty,
    envExtend,
    envLookup,
    stateInitial,
    askEnv,
    modifyEffects,
    localEnv,
    getState,
    putState,
    modifyState,
    evalPrint,
    catch,
    failure,
    evalKvGet,
    evalKvPut,
    transaction,
    EvalM,
    Val (..),
    EvalOp (..),
    Free (..),
    Error,
    Env,
    State,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Error = String

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

data Free e a
  = Pure a
  | Free (e (Free e a))

instance (Functor e) => Functor (Free e) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free g) = Free $ fmap (fmap f) g

instance (Functor e) => Applicative (Free e) where
  pure = Pure
  (<*>) = ap

instance (Functor e) => Monad (Free e) where
  Pure x >>= f = f x
  Free g >>= f = Free $ h <$> g
    where
      h x = x >>= f

data EvalOp a
  = ReadOp (Env -> a)
  | StateGetOp (State -> a)
  | StatePutOp State a
  | PrintOp String a
  | ErrorOp Error
  | TryCatchOp a a
  | KvGetOp Val (Val -> a)
  | KvPutOp Val Val a
  | TransactionOp (EvalM ()) a

instance Functor EvalOp where
  fmap f (ReadOp k) = ReadOp $ f . k
  fmap f (StateGetOp k) = StateGetOp $ f . k
  fmap f (StatePutOp s m) = StatePutOp s $ f m
  fmap f (PrintOp p m) = PrintOp p $ f m
  fmap f (TryCatchOp r l) = TryCatchOp (f r) $ f l
  fmap f (KvGetOp key k) = KvGetOp key $ f . k
  fmap f (KvPutOp key val m) = KvPutOp key val $ f m
  fmap f (TransactionOp void m) = TransactionOp void $ f m 
  fmap _ (ErrorOp e) = ErrorOp e

type EvalM a = Free EvalOp a

askEnv :: EvalM Env
askEnv = Free $ ReadOp $ \env -> pure env

modifyEffects :: (Functor e, Functor h) => (e (Free e a) -> h (Free e a)) -> Free e a -> Free h a
modifyEffects _ (Pure x) = Pure x
modifyEffects g (Free e) = Free $ modifyEffects g <$> g e

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f = modifyEffects g
  where
    g (ReadOp k) = ReadOp $ k . f
    g op = op

getState :: EvalM State
getState = Free $ StateGetOp $ \s -> pure s

putState :: State -> EvalM ()
putState s = Free $ StatePutOp s $ pure ()

modifyState :: (State -> State) -> EvalM ()
modifyState f = do
  s <- getState
  putState $ f s

evalPrint :: String -> EvalM ()
evalPrint p = Free $ PrintOp p $ pure ()

failure :: String -> EvalM a
failure = Free . ErrorOp

catch :: EvalM a -> EvalM a -> EvalM a
catch (Free (ErrorOp _)) handler = handler
catch (Free (ReadOp k)) handler = Free (ReadOp (\env -> catch (k env) handler))
catch (Free (StateGetOp k)) handler = Free (StateGetOp (\state -> catch (k state) handler))
catch (Free (StatePutOp state m)) handler = Free (StatePutOp state (catch m handler))
catch (Free (PrintOp p m)) handler = Free (PrintOp p (catch m handler))
catch (Free (TryCatchOp tryOp catchOp)) handler = Free (TryCatchOp (catch tryOp catchOp) (catch catchOp handler))

catch try _ = try

evalKvGet :: Val -> EvalM Val
evalKvGet key = do
  state <- getState
  case lookup key state of
    Just val -> pure val
    Nothing -> failure "Key not found"

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut key val = modifyState (\s -> (key, val) : filter ((/= key) . fst) s)

transaction :: EvalM () -> EvalM ()
transaction evalmvoid = Free $ TransactionOp evalmvoid (pure ())

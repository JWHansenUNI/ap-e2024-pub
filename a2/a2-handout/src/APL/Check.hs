{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module APL.Check (runCheck, checkExp, String, CheckM) where

import APL.AST (Exp (..), VName)
import APL.Eval (eval)

type Scope = [String]

newtype CheckM a = CheckM (Scope -> (Scope, Maybe a))

instance Functor CheckM where
    fmap f (CheckM g) = CheckM $ \s -> let
        (s', ma) = g s
        in (s', fmap f ma)

instance Applicative CheckM where
    pure x = CheckM $ \s -> (s, Just x)
    CheckM ff <*> CheckM fx = CheckM $ \s ->
        let (s', mf) = ff s
            (s'', mx) = fx s'
        in (s'', mf <*> mx)

instance Monad CheckM where
    return = pure
    CheckM c >>= f = CheckM $ \s ->
        let (s', ma) = c s
        in case ma of
            Nothing -> (s', Nothing)
            Just a -> let CheckM c' = f a in c' s'

runCheck :: CheckM (Maybe String) -> Maybe String
runCheck (CheckM m) = case snd (m []) of
    Nothing -> Nothing
    Just result -> result

extendScope :: CheckM (Maybe String) -> String -> CheckM (Maybe String)
extendScope (CheckM c) vname = CheckM $ \s ->
    let (s', m) = c s
    in (vname : s', m)

scopeLookup :: String -> CheckM (Maybe String)
scopeLookup vname = CheckM $ \s ->
    if vname `elem` s
    then (s, Nothing)  -- No error, return Nothing
    else (s, Just (Just ("Variable not in scope: " ++ show vname)))

checkExp :: Exp -> CheckM (Maybe String)
checkExp (Let vname e1 e2) = do
    -- Extend the scope with the new variable and pass the updated scope to checkExp
    extendScope (pure Nothing) vname >> checkExp e2

checkExp (Var vname) = do
    -- Lookup the variable in the scope
    scopeLookup vname

checkExp (Add e1 e2) = do
    v1 <- checkExp e1
    v2 <- checkExp e2
    case (v1, v2) of
        (Nothing, Nothing) -> return Nothing
        (Just err, _) -> return (Just err)
        (_, Just err) -> return (Just err)
checkExp (CstInt _) = return Nothing
checkExp (CstBool _) = return Nothing
checkExp (Lambda vname e1) =
    extendScope (pure Nothing) vname >> checkExp e1

checkExp _ = return (Just "")  -- Handle other cases


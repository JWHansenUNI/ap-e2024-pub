module APL.Tests
  ( properties
  )
where

import APL.AST (Exp (..), subExp, VName)
import APL.Error (isVariableError, isDomainError, isTypeError, Error (UnknownVariable, NonInteger, DivisionByZero, NegativeExponent, InvalidEqual, NonBoolean, NonFunction))
import APL.Check (checkExp)
import APL.Parser (parseAPL)
import APL.Eval
import Test.QuickCheck
  ( Property
  , Gen
  , Arbitrary (arbitrary, shrink)
  , property
  , cover
  , checkCoverage
  , oneof
  , sized
  , elements
  , frequency
  , generate
  )

instance Arbitrary Exp where
  arbitrary = sized (genExp ["var"])

  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) =
    e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) =
    e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) =
    e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) =
    e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) =
    e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) =
    e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) =
    e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink _ = []

genExp :: [VName] -> Int -> Gen Exp
genExp _ 0 =
  frequency
    [ (1, CstInt <$> arbitrary)
    , (1, CstBool <$> arbitrary)
    ]
genExp vars size =
  frequency
    [ (2, CstInt <$> arbitrary)  -- Increase type-safe constants
    , (2, CstBool <$> arbitrary) -- Increase type-safe constants
    , (2, Add <$> genExp vars halfSize <*> genExp vars halfSize)  -- Type error risk, lower frequency
    , (2, Sub <$> genExp vars halfSize <*> genExp vars halfSize)  -- Type error risk, lower frequency
    , (1, Mul <$> genExp vars halfSize <*> genExp vars halfSize)  -- Type error risk, much lower
    , (1, Div <$> genExp vars halfSize <*> divExp)                -- Domain error focus, lower frequency
    , (1, Pow <$> genExp vars halfSize <*> powExp)                -- Domain error focus, lower frequency
    , (2, Eql <$> genExp vars halfSize <*> genExp vars halfSize)  -- Type error risk, keep moderate
    , (1, If <$> genExp vars thirdSize <*> genExp vars thirdSize <*> genExp vars thirdSize)  -- Type safe if properly used
    , (2, Var <$> genVar vars)                                    -- Type safe variable generation
    , (24, do  -- Increase frequency of Let, but make sure it's mostly type safe
          var <- generatedArbVar
          Let var <$> genExp (var : vars) halfSize <*> genExp vars halfSize
      )
    , (24, do  -- Increase frequency of Lambda, ensure type safety
          var <- generatedArbVar
          Lambda var <$> genExp (var : vars) (size - 1)
      )
    , (1, Apply <$> genExp vars halfSize <*> genExp vars halfSize) -- Type error risk, lower frequency
    , (1, TryCatch <$> genExp vars halfSize <*> genExp vars halfSize)
    ]
  where
    halfSize = size `div` 2
    thirdSize = size `div` 3
    generatedArbVar = genVar vars

genVar :: [VName] -> Gen VName
genVar [] =
  frequency
  [
    (2, arbitrary),
    (98, combineThree ["x", "y", "z", "q", "h", "s"])
  ]
genVar vars = frequency
  [
    (70, elements vars),
    (30, frequency [
      (0, arbitrary),
      (100, combineThree ["x", "y", "z", "q", "h", "s", "t", "l"])
      ])
  ]

-- Special case for generating a denominator that is likely to be zero
divExp :: Gen Exp
divExp = do
  denominator <- frequency [(10, pure 0), (90, arbitrary)]
  numerator <- arbitrary
  return (Div (CstInt numerator) (CstInt denominator))

-- Special case for generating a power with a negative exponent
powExp :: Gen Exp
powExp = do
  base <- arbitrary
  exponent <- frequency [(20, pure (-1)), (80, arbitrary)]
  return (Pow (CstInt base) (CstInt exponent))


combineThree :: [VName] -> Gen VName
combineThree chars = do
  v1 <- elements chars
  v2 <- elements chars
  v3 <- elements chars
  return (v1 ++ v2 ++ v3)


expCoverage :: Exp -> Property
expCoverage e = checkCoverage
  . cover 20 (any isDomainError (checkExp e)) "domain error"
  . cover 20 (not $ any isDomainError (checkExp e)) "no domain error"
  . cover 20 (any isTypeError (checkExp e)) "type error"
  . cover 20 (not $ any isTypeError (checkExp e)) "no type error"
  . cover 5 (any isVariableError (checkExp e)) "variable error"
  . cover 70 (not $ any isVariableError (checkExp e)) "no variable error"
  . cover 50 (or [2 <= n && n <= 4 | Var v <- subExp e, let n = length v]) "non-trivial variable"
  $ ()

parsePrinted :: Exp -> Bool
parsePrinted e1 =
  let printed = show e1
  in case parseAPL "" printed of
       Right e2 -> e2 == e1 -- Check that parsed expression matches the original
       Left _ -> False       -- Return False if parsing fails

addReplaceErrorToList :: Error -> [Error] -> [Error]
addReplaceErrorToList e es
  | e `elem` es = es
  | otherwise = e:es

makeErrorList :: Exp -> [Error] -> [Error]
makeErrorList e1 errorList =
  case e1 of
    CstInt _ -> errorList
    CstBool _ -> errorList
    Var v -> addReplaceErrorToList (UnknownVariable v) errorList
    Add e1 e2 -> 
      let
        newErrorList = addReplaceErrorToList NonInteger errorList
      in
      makeErrorList e1 $ makeErrorList e2 newErrorList
    Sub e1 e2 ->
      let
        newErrorList = addReplaceErrorToList NonInteger errorList
      in makeErrorList e1 $ makeErrorList e2 newErrorList
    Mul e1 e2 -> let
      newErrorList = addReplaceErrorToList NonInteger errorList
      in makeErrorList e1 $ makeErrorList e2 newErrorList
    Div e1 e2 -> let
      newErrorList = addReplaceErrorToList NonInteger $ addReplaceErrorToList DivisionByZero errorList
      in makeErrorList e1 $ makeErrorList e2 newErrorList
    Pow e1 e2 -> let
      newErrorList = addReplaceErrorToList NonInteger $ addReplaceErrorToList NegativeExponent errorList
      in makeErrorList e1 $ makeErrorList e2 newErrorList
    Eql e1 e2 -> let
      newErrorList = addReplaceErrorToList InvalidEqual errorList
      in makeErrorList e1 $ makeErrorList e2 newErrorList
    If e1 e2 e3 -> let
      newErrorList = addReplaceErrorToList NonBoolean errorList
      in makeErrorList e1 $ makeErrorList e2 $ makeErrorList e2 newErrorList
    Let v e1 e2 ->makeErrorList e1 $ makeErrorList e2 errorList
    Lambda v e1 -> makeErrorList e1 errorList
    Apply e1 e2 -> let
      newErrorList = addReplaceErrorToList NonFunction errorList
      in makeErrorList e1 $ makeErrorList e2 newErrorList
    TryCatch e1 e2 -> makeErrorList e1 $ makeErrorList e2 errorList

--Checks that all elements in one array exists in the other
checkExist :: [Error] -> [Error] -> Bool
checkExist [] [] = True
checkExist [] ys = True
checkExist xs [] = False
checkExist [x] ys = x `elem` ys
checkExist (x:xs) ys = do
  case x `elem` ys of
    True -> checkExist xs ys
    otherwise -> False

--Makes sure to check that all elements in one array exists in the other and vice verca
onlyCheckedErrors2 :: Exp -> Bool
onlyCheckedErrors2 e1 =
  let expectedErrorList = makeErrorList e1 []
      actualErrorList = checkExp e1
  in (
      checkExist expectedErrorList actualErrorList && checkExist actualErrorList expectedErrorList
    )

onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors e1 = do
  let actualErrorList = checkExp e1
  case runEval (eval e1) of
    Left err -> err `elem` actualErrorList
    Right result -> True


properties :: [(String, Property)]
properties =
  [ ("expCoverage", property expCoverage)
  , ("onlyCheckedErrors", property onlyCheckedErrors)
  , ("parsePrinted", property parsePrinted)
  ]

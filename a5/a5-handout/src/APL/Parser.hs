module APL.Parser (parseAPL) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    satisfy,
    some,
    try,
    optional
  )
import Text.Megaparsec.Char (space)

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = p <* space

keywords :: [String]
keywords =
  [ "CstInt",
    "CstBool",
    "Var",
    "Lambda",
    "Let",
    "If",
    "TryCatch",
    "Apply",
    "Eql",
    "False",
    "True",
    "Pow",
    "Add",
    "Sub",
    "Div"
  ]

lVName :: Parser VName
lVName = lexeme $ try $ do
  lString "\""           -- consume the opening quote
  c <- satisfy isAlpha   -- the first character must be alphabetic
  cs <- many $ satisfy isAlphaNum -- remaining characters are alphanumeric
  lString "\""           -- consume the closing quote
  let v = c : cs
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v

lInteger :: Parser Integer
lInteger = lexeme $ do
  sign <- optional (satisfy (== '-'))
  num <- read <$> some (satisfy isDigit)
  let value = case sign of
        Just _ -> -num
        Nothing -> num
  pure value

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

lBool :: Parser Bool
lBool =
  lexeme . try . choice $
    [ const True <$> lKeyword "True",
      const False <$> lKeyword "False"
    ]

pAtom :: Parser Exp
pAtom =
  choice
    [
      lKeyword "CstInt" *> (CstInt <$> (optional (lString "(") *> lInteger <* optional (lString ")"))),
      lKeyword "CstBool" *> (CstBool <$> lBool),
      lKeyword "Var" *> (Var <$> lVName),
      lString "(" *> pExp <* lString ")"
    ]

pExp :: Parser Exp
pExp = choice
          [ do
              lKeyword "Eql"
              e1 <- pExp
              e2 <- pExp
              pure (Eql e1 e2),
            do
              lKeyword "Add"
              e1 <- pExp
              e2 <- pExp
              pure (Add e1 e2),
            do
              lKeyword "Sub"
              e1 <- pExp
              e2 <- pExp
              pure (Sub e1 e2),
            do
              lKeyword "Mul"
              e1 <- pExp
              e2 <- pExp
              pure (Mul e1 e2),
            do
              lKeyword "Div"
              e1 <- pExp
              e2 <- pExp
              pure (Div e1 e2),
            do
              lKeyword "Pow"
              e1 <- pExp
              e2 <- pExp
              pure (Pow e1 e2),
            do
              lKeyword "If"
              e1 <- pExp
              e2 <- pExp
              e3 <- pExp
              pure (If e1 e2 e3),
            do
              lKeyword "Lambda"
              vname <- lVName
              e1 <- pExp
              pure (Lambda vname e1),
            do
              lKeyword "TryCatch"
              e1 <- pExp
              e2 <- pExp
              pure (TryCatch e1 e2),
            do
              lKeyword "Let"
              vname <- lVName
              e1 <- pExp
              e2 <- pExp
              pure (Let vname e1 e2),
            do
              lKeyword "Apply"
              e1 <- pExp
              e2 <- pExp
              pure (Apply e1 e2),
            pAtom
          ]


parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x

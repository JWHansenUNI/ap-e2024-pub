module APL.Parser (parseAPL) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    between,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (char, space)

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = p <* space

keywords :: [String]
keywords =
  [ "if",
    "then",
    "else",
    "true",
    "false",
    "let",
    "in",
    "try",
    "catch",
    "print",
    "put",
    "get"
  ]

lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c : cs
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v

lInteger :: Parser Integer
lInteger =
  lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlphaNum)

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

lSomeString :: Parser String
lSomeString = lexeme $ between (char '"') (char '"') (some $ satisfy isAlphaNum)

lBool :: Parser Bool
lBool =
  lexeme . try . choice $
    [ const True <$> lKeyword "true",
      const False <$> lKeyword "false"
    ]

pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> lBool,
      Var <$> lVName,
      lString "(" *> pExp <* lString ")"
    ]

pFExp :: Parser Exp
pFExp = do
  func <- pAtom -- Gets the function
  args <- many pAtom -- Gets the arguments 
  pure $ foldl Apply func args -- Applies the arguments to the function from left to right

pLExp :: Parser Exp
pLExp =
  choice
    [
      do
        lKeyword "print"
        s <- lSomeString
        Print s <$> pAtom,
      do
        lKeyword "get"
        KvGet <$> pAtom,
      do
        lKeyword "put"
        x <- pAtom
        KvPut x <$> pAtom,
      do 
        lString "'\'"
        arg <- lVName
        lString "->"
        Lambda arg <$> pExp,
      do 
        lKeyword "try"
        first <- pExp
        lKeyword "catch"
        TryCatch first <$> pExp,
      do 
        lKeyword "let"
        v1 <- lVName
        lString "="
        e1 <- pExp
        lKeyword "in"
        Let v1 e1 <$> pExp,
      pFExp,
      If
        <$> (lKeyword "if" *> pExp)
        <*> (lKeyword "then" *> pExp)
        <*> (lKeyword "else" *> pExp),
      pAtom
    ]

pExp2 :: Parser Exp
pExp2 = pLExp >>= chain
  where
    chain x =
      choice [
        do
          lString "**"
          y <- pLExp -- Jump all the way to pAtom as it will either be a number or an expression in a parenthesis
          chain $ Pow x y,
        pure x
      ]

pExp1 :: Parser Exp
pExp1 = pExp2 >>= chain
  where
    chain x =
      choice
        [ do
            lString "*"
            y <- pExp2
            chain $ Mul x y,
          do
            lString "/"
            y <- pExp2
            chain $ Div x y,
          pure x
        ]

pExp0 :: Parser Exp
pExp0 = pExp1 >>= chain
  where
    chain x =
      choice
        [ do
            lString "+"
            y <- pExp1
            chain $ Add x y,
          do
            lString "-"
            y <- pExp1
            chain $ Sub x y,
          pure x
        ]

pExp :: Parser Exp
pExp =
  pExp0 >>= chain
  where
    chain x =
      choice [
        do
          lString "=="
          y <- pExp0
          chain $ Eql x y,
        pure x
      ]

parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x

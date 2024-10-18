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
    parseTest,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

-- Do not change this definition.
type Parser = Parsec Void String

keywords :: [String]
keywords = 
  [
    "if",
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

lInteger :: Parser Integer
lInteger = lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlpha)

lexeme :: Parser a -> Parser a
lexeme p = p <* space

lVname :: Parser VName
lVname = lexeme $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c : cs
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v
  pure $ c:cs

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

pBool :: Parser Bool
pBool = choice $ 
  [
    const True <$> lKeyword "true",
    const False <$> lKeyword "false"
  ]

pAtom :: Parser Exp
pAtom = 
  choice [
    CstInt <$> lInteger,
    CstBool <$> pBool,
    Var <$> lVname,
    lString "(" *> pExp <* lString ")"
  ]

pExp1 :: Parser Exp
pExp1 = pLExp >>= chain
  where
    chain x =
      choice [
        do 
          lString "*"
          y <- pLExp
          chain $ Mul x y,
        do
          lString "/"
          y <- pLExp
          chain $ Div x y,
        pure x
      ]

pLExp :: Parser Exp
pLExp =
  choice
    [
      If
        <$> (lKeyword "if" *> pExp0)
        <*> (lKeyword "then" *> pExp0)
        <*> (lKeyword "else" *> pExp0),
      pAtom
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
pExp = pExp0
-- Do not change this definition.
parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x

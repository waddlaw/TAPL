{-# LANGUAGE OverloadedStrings #-}

module Language.FJ.Parser
  ( Parser,
    runFjParser,
    pClassDef,
    pTerm,
  )
where

import Language.FJ.Type
import RIO hiding (many, some, try)
import qualified RIO.Text as Text
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

runFjParser :: String -> Either String Term
runFjParser input = case runParser pTerm "" (Text.pack input) of
  Left bundle -> Left (errorBundlePretty bundle)
  Right x -> Right x

-- | Class declaration parser
pClassDef :: Parser ClassDef
pClassDef =
  CL
    <$ pKeyword "class"
    <*> pClass
    <* pKeyword "extends"
    <*> pClass
    <* symbol "{"
    -- body of class definition
    <*> many (try (pFieldDef <* symbol ";"))
    <*> pConstDef
    <*> many pMethodDef
    <* symbol "}"

pFieldDef :: Parser (Class, Field)
pFieldDef = (,) <$> pClass <*> pField

-- | Constructor declaration parser
pConstDef :: Parser ConstDef
pConstDef = do
  cls <- pClass
  args <- pArgs pFieldDef
  (super, this) <- pBody pConstDefBody
  return (K cls args super this)

pConstDefBody :: Parser ([Field], [(Field, Field)])
pConstDefBody =
  (,)
    <$ pKeyword "super"
    <*> pArgs pField
    <* symbol ";"
    <*> endBy pFieldAssign (symbol ";")

pFieldAssign :: Parser (Field, Field)
pFieldAssign =
  (,)
    <$ pVar -- this
    <* string "."
    <*> pField
    <* symbol "="
    <*> pField

-- | Method declaration parser
pMethodDef :: Parser MethodDef
pMethodDef =
  mkMethodDef
    <$> pClass
    <*> pMethod
    <*> pArgs pMethodArg
    <*> pBody (pKeyword "return" *> pTerm <* symbol ";")

pMethodArg :: Parser (Class, Var)
pMethodArg = (,) <$> pClass <*> pVar

pArgs :: Parser a -> Parser [a]
pArgs p = between (symbol "(") (symbol ")") (sepBy p (symbol ","))

pBody :: Parser a -> Parser a
pBody = between (symbol "{") (symbol "}")

pName :: (Text -> b) -> Parser b
pName f = do
  c <- lowerChar
  cs <- many alphaNumChar
  return . f $ Text.pack (c : cs)

-- | Term parser
pTerm :: Parser Term
pTerm = do
  let p = choice [pTermNew, pTermCast, pTermVar]
  t <- try (between (symbol "(") (symbol ")") p) <|> p
  pTerm' t <|> return t

pTerm' :: Term -> Parser Term
pTerm' t = do
  _ <- string "."
  try (pTermMethod t) <|> pTermField t

-- | Term variable parser
pTermVar :: Parser Term
pTermVar = TmVar <$> pVar

pTermNew :: Parser Term
pTermNew =
  TmNew
    <$ try (pKeyword "new")
    <*> pClass
    <*> pArgs pTerm

pTermField :: Term -> Parser Term
pTermField t = TmFieldRef t <$> pField

pTermMethod :: Term -> Parser Term
pTermMethod t = TmMethodInv t <$> pMethod <*> pArgs pTerm

pTermCast :: Parser Term
pTermCast = TmCast <$> between (symbol "(") (symbol ")") pClass <*> pTerm

-- | Class name parser
pClass :: Parser Class
pClass = lexeme $ do
  c <- upperChar
  cs <- many alphaNumChar
  return . mkClass $ Text.pack (c : cs)

-- | Field name parser
pField :: Parser Field
pField = pName mkField

-- | Method name parser
pMethod :: Parser Method
pMethod = pName mkMethod

-- | Variable name parser
pVar :: Parser Var
pVar = pName mkVar

-- utils
pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

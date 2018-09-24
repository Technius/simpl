{-# LANGuAGE OverloadedStrings #-}
module Simpl.Parser where

import Control.Monad.Combinators
import Control.Monad.Combinators.Expr
import Data.Functor.Foldable (Fix(Fix))
import Data.Void
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (pretty)
import Text.Megaparsec
import qualified Data.Text as Text
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Simpl.Ast (Expr, Decl, SourceFile, Type)
import qualified Simpl.Ast as Ast

type Parser m a = ParsecT Void Text m a

whitespace :: Parser m ()
whitespace = L.space C.space1 (L.skipLineComment "#") empty

lexeme :: Parser m a -> Parser m a
lexeme = L.lexeme whitespace

symbol :: Text -> Parser m Text
symbol = L.symbol whitespace

parens :: Parser m a -> Parser m a
parens = between (C.char '(') (C.char ')')

reservedKeywords :: [Text]
reservedKeywords = ["fun", "data", "if", "then", "else", "true", "false"]

keyword :: Text -> Parser m Text
keyword k = lexeme (C.string k <* notFollowedBy C.alphaNumChar)

identifier :: Parser m Text
identifier = lexeme go >>= check
  where
    check :: Text -> Parser m Text
    check w =
      if w `elem` reservedKeywords
        then fail $ Text.unpack w ++ " is a reserved keyword"
        else pure w
    go :: Parser m Text
    go = Text.cons <$> (C.letterChar <|> C.char '_')
      <*> (Text.pack <$> many (C.alphaNumChar <|> C.char '_'))

literal :: Parser m Expr
literal = lexeme (bool <|> number)
  where
    bool = Ast.litBool <$> ((symbol "true" >> pure True) <|> (symbol "false" >> pure False))
    number = Ast.litDouble <$> (try (signed L.float) <|> decimal)
    signed = L.signed whitespace
    decimal = signed (fromIntegral <$> (L.decimal :: Parser m Int))

-- | Non-recursive component of expression gramamr
atom :: Parser m Expr
atom = literal

-- | Arithmetic expression parser
arith :: Parser m Expr
arith = makeExprParser (lexeme (parens expr) <|> atom) arithTable
  where
    arithTable =
      [ [ binary "*" Ast.mul
        , binary "/" Ast.div ]
      , [ binary "+" Ast.add
        , binary "-" Ast.sub ] ]
    binary name f = InfixL (f <$ symbol name)

ifExpr :: Parser m Expr
ifExpr = lexeme $ do
  _ <- keyword "if"
  cond <- expr
  _ <- keyword "then"
  t1 <- expr
  _ <- keyword "else"
  t2 <- expr
  pure $ Ast.ifExpr cond t1 t2

-- | ADT constructor
adtCons :: Parser m Expr
adtCons = lexeme $ do
  name <- typeIdentifier
  args <- many (lexeme (parens expr) <|> atom)
  pure $ Ast.cons name args

expr :: Parser m Expr
expr = try ifExpr <|> try adtCons <|> arith

typeLit :: Parser m Type
typeLit = Fix <$> ((symbol "Double" >> pure Ast.TyDouble)
          <|> (symbol "Bool" >> pure Ast.TyBool))

typeIdentifier :: Parser m Text
typeIdentifier = lexeme (Text.pack <$> ((:) <$> C.upperChar <*> many C.alphaNumChar))

typeAdt :: Parser m Type
typeAdt = Fix . Ast.TyAdt <$> typeIdentifier

type' :: Parser m Type
type' = typeLit <|> typeAdt

declFun :: Parser m (Decl Expr)
declFun = lexeme $ do
  _ <- keyword "fun"
  name <- identifier
  _ <- symbol ":"
  ty <- type'
  _ <- symbol ":="
  body <- between (symbol "{") (symbol "}") expr
  pure $ Ast.DeclFun name ty body

constructor :: Parser m Ast.Constructor
constructor = lexeme $ do
  name <- typeIdentifier
  args <- many type'
  pure $ Ast.Ctor name args

declAdt :: Parser m (Decl Expr)
declAdt = do
  _ <- keyword "data"
  name <- typeIdentifier
  _ <- symbol "="
  ctors <- constructor `sepBy1` symbol "|"
  pure $ Ast.DeclAdt name ctors

decl :: Parser m (Decl Expr)
decl = declFun <|> declAdt

sourceFile :: Text -> Parser m (SourceFile Expr)
sourceFile name = (Ast.SourceFile name <$> many decl) <* eof

runExprParser :: Text -> Either String Expr
runExprParser input = case runParser (expr <* eof) "" input of
  Left err -> Left (parseErrorPretty' input err)
  Right e -> Right e

testRunExprParser :: String -> Either String String
testRunExprParser = fmap (show . pretty) . runExprParser . Text.pack
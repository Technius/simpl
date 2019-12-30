{-# LANGUAGE DataKinds #-}
{-# LANGuAGE OverloadedStrings #-}
module Simpl.Parser where

import Control.Applicative (liftA2)
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

import Simpl.Annotation (Fields(ExprPos), withPos, toAnnExprF, addField, unannotate)
import Simpl.Ast (Decl, SourceFile, Branch)
import qualified Simpl.Ast as Ast
import Simpl.Type (Type, Numeric(..), TypeF(..))

type Parser m a = ParsecT Void Text m a

type SourcedExpr = Ast.AnnExpr '[ 'ExprPos ]

whitespace :: Parser m ()
whitespace = L.space C.space1 (L.skipLineComment "#") empty

lexeme :: Parser m a -> Parser m a
lexeme = L.lexeme whitespace

symbol :: Text -> Parser m Text
symbol = L.symbol whitespace

parens :: Parser m a -> Parser m a
parens = between (C.char '(') (C.char ')')

signed :: Num a => Parser m a -> Parser m a
signed = L.signed whitespace

withSourcePos :: (SourcePos -> Parser m a) -> Parser m a
withSourcePos f = getPosition >>= f

tagSourcePos :: Parser m (Ast.ExprF SourcedExpr)
             -> Parser m SourcedExpr
tagSourcePos m = withSourcePos $ \pos -> do
  m' <- m
  pure . Fix . addField (withPos pos) . toAnnExprF $ m'

reservedKeywords :: [Text]
reservedKeywords = ["fun", "data", "if", "then", "else", "true", "false", "case", "of", "let", "in", "cast", "as", "println", "extern"]

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
    go = Text.cons <$> (C.lowerChar <|> C.char '_')
      <*> (Text.pack <$> many (C.alphaNumChar <|> C.char '_'))

literal :: Parser m SourcedExpr
literal = lexeme (bool <|> number <|> str)
  where
    lit = tagSourcePos . fmap Ast.Lit
    bool = lit $ Ast.LitBool <$> ((symbol "true" >> pure True) <|> (symbol "false" >> pure False))
    number = try double <|> integer
    double = lit $ Ast.LitDouble <$> signed L.float
    integer = lit $ Ast.LitInt <$> signed L.decimal
    quoteChar :: Parser m Char    -- avoid type variable ambiguity
    quoteChar = C.char '"'
    str = lit $ Ast.LitString . Text.pack <$> (quoteChar >> manyTill L.charLiteral quoteChar)

var :: Parser m SourcedExpr
var = tagSourcePos (Ast.Var <$> identifier)

funRef :: Parser m SourcedExpr
funRef = lexeme (tagSourcePos (Ast.FunRef <$> (C.char '&' >> identifier)))

appExpr :: Parser m SourcedExpr
appExpr = lexeme . tagSourcePos $ do
  fname <- C.string "@" >> identifier
  args <- option [] (parens (expr `sepBy` symbol ","))
  pure $ Ast.App fname args

-- | Non-recursive component of expression gramamr
atom :: Parser m SourcedExpr
atom = appExpr <|> funRef <|> literal <|> var

-- | Arithmetic expression parser
arith :: Parser m SourcedExpr
arith = makeExprParser (lexeme (parens expr) <|> atom) arithTable
  where
    arithTable =
      [ [ binary "*"  Ast.Mul
        , binary "/"  Ast.Div ]
      , [ binary "+"  Ast.Add
        , binary "-"  Ast.Sub ]
      , [ binary "<=" Ast.Lte
        , binary "<"  Ast.Lt
        , binary "==" Ast.Equal ] ]
    binary name op = InfixL $ withSourcePos $ \pos -> do
      _ <- symbol name
      pure $ \a b -> Fix . addField (withPos pos) $ toAnnExprF (Ast.BinOp op a b)

ifExpr :: Parser m SourcedExpr
ifExpr = tagSourcePos . lexeme $ do
  _ <- keyword "if"
  cond <- expr
  _ <- keyword "then"
  t1 <- expr
  _ <- keyword "else"
  t2 <- expr
  pure $ Ast.If cond t1 t2

-- | ADT constructor
adtCons :: Parser m SourcedExpr
adtCons = tagSourcePos . lexeme $ do
  name <- typeIdentifier
  args <- many (lexeme (parens expr) <|> try atom)
  pure $ Ast.Cons name args

-- | Case analysis branch
branch :: Parser m (Branch SourcedExpr)
branch = lexeme $ do
  name <- typeIdentifier
  args <- many identifier
  _ <- symbol "=>"
  body <- expr
  pure $ Ast.BrAdt name args body

-- | Case analysis expression
caseExpr :: Parser m SourcedExpr
caseExpr = tagSourcePos . lexeme $ do
  _ <- symbol "case"
  val <- expr
  _ <- symbol "of"
  branches <- some branch
  pure $ Ast.Case branches val

letExpr :: Parser m SourcedExpr
letExpr = tagSourcePos . lexeme $ do
  _ <- symbol "let"
  name <- identifier
  _ <- symbol "="
  val <- expr
  _ <- symbol "in"
  Ast.Let name val <$> expr

castExpr :: Parser m SourcedExpr
castExpr = tagSourcePos . lexeme $ do
  _ <- symbol "cast"
  e <- expr
  _ <- symbol "as"
  n <- numeric
  pure $ Ast.Cast e n

printExpr :: Parser m SourcedExpr
printExpr = tagSourcePos . lexeme $ (symbol "println" >> Ast.Print <$> parens expr)

expr :: Parser m SourcedExpr
expr = letExpr <|> caseExpr <|> ifExpr <|> castExpr <|> printExpr <|> try adtCons <|> arith

numeric :: Parser m Numeric
numeric = (symbol "Double" >> pure NumDouble) <|> (symbol "Int" >> pure NumInt)

typeLit :: Parser m Type
typeLit = Fix <$> ((TyNumber <$> numeric)
                   <|> (symbol "Bool" >> pure TyBool)
                   <|> (symbol "String" >> pure TyString))

typeIdentifier :: Parser m Text
typeIdentifier = lexeme (Text.pack <$> ((:) <$> C.upperChar <*> many C.alphaNumChar))

typeVar :: Parser m Type
typeVar = Fix . TyVar <$> identifier

typeAdt :: Parser m Type
typeAdt = do
  name <- typeIdentifier
  tparams <- many (parens type' <|> type')
  pure $ Fix (TyAdt name tparams)

typeAtom :: Parser m Type
typeAtom = typeLit <|> typeAdt <|> typeVar

typeFun :: Parser m Type
typeFun = lexeme $ do
  first <- (parens type' <|> typeAtom)
  _ <- symbol "->"
  rest <- (parens type' <|> typeAtom) `sepBy1` symbol "->"
  pure . Fix $ TyFun (first : init rest) (last rest)

type' :: Parser m Type
type' = try typeFun <|> typeAtom <?> "type"

declFunParamList :: Parser m [(Text, Type)]
declFunParamList = lexeme $ option [] (parens params)
  where
    oneParam = liftA2 (,) (identifier <* symbol ":") type'
    params = oneParam `sepBy1` symbol ","

declFunDecl :: Parser m (Text, [(Text, Type)], Type)
declFunDecl = do
  _ <- keyword "fun"
  name <- identifier
  params <- declFunParamList
  _ <- symbol ":"
  ty <- type'
  _ <- symbol ":="
  pure (name, params, ty)

declFun :: Parser m (Decl SourcedExpr)
declFun = lexeme $
  declFunDecl >>= (\x -> choice [f x | f <- [declFunStatic, declFunExtern]])
  where
    declFunStatic (name, params, ty) = do
      body <- between (symbol "{") (symbol "}") expr
      pure $ Ast.DeclFun name params ty body
    declFunExtern (n, p, t) = keyword "extern" >> (pure $ Ast.DeclExtern n p t)

constructor :: Parser m Ast.Constructor
constructor = lexeme $ do
  name <- typeIdentifier
  args <- many (parens type' <|> type')
  pure $ Ast.Ctor name args

declAdt :: Parser m (Decl SourcedExpr)
declAdt = do
  _ <- keyword "data"
  name <- typeIdentifier
  tparams <- many identifier
  _ <- symbol "="
  _ <- symbol "{"
  ctors <- constructor `sepBy1` symbol "|"
  _ <- symbol "}"
  pure $ Ast.DeclAdt name tparams ctors

decl :: Parser m (Decl SourcedExpr)
decl = declFun <|> declAdt

sourceFile :: Text -> Parser m (SourceFile SourcedExpr)
sourceFile name = (Ast.SourceFile name <$> many decl) <* eof

runExprParser :: Text -> Either String SourcedExpr
runExprParser input = case runParser (expr <* eof) "" input of
  Left err -> Left (parseErrorPretty' input err)
  Right e -> Right e

testRunExprParser :: String -> Either String String
testRunExprParser = fmap (show . pretty . unannotate) . runExprParser . Text.pack

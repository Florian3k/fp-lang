{-# LANGUAGE RecordWildCards #-}

module Parser where

import AST
import Control.Monad (guard)
import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Data.Functor (($>))
import Data.Void (Void)
import Text.Megaparsec (Parsec, empty, many, notFollowedBy, oneOf, optional, satisfy, sepBy, some, try, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L (lexeme, skipLineComment, space)

type Parser = Parsec Void String

programParser :: Parser [Statement]
programParser = some statementP

-- Statements

statementP :: Parser Statement
statementP = dataStmtP <|> funcStmtP <|> classStmtP <|> instanceStmtP
  where
    dataStmtP = DataStmt <$> dataDefP
    funcStmtP = FuncStmt <$> funcDefP
    classStmtP = ClassStmt <$> classDefP
    instanceStmtP = InstanceStmt <$> instanceDefP

dataDefP :: Parser DataDef
dataDefP = do
  keyword "data"
  name <- upperIdP
  typeParams <- typeParamsP
  lexChar '{'
  ctors <- many typeConstructor
  lexChar '}'
  pure $ DataDef {..}
  where
    typeConstructor :: Parser (UpperId, [Type])
    typeConstructor = do
      name <- upperIdP
      lexChar '('
      types <- sepBy typeP (lexChar ',')
      lexChar ')'
      pure (name, types)

funcDefP :: Parser FuncDef
funcDefP = do
  keyword "fun"
  name <- lowerIdP
  typeParams <- typeParamsP
  instances <- instancesP
  lexChar '('
  params <- sepBy funParamP (lexChar ',')
  lexChar ')'
  lexChar ':'
  retType <- typeP
  lexChar '='
  expr <- exprP
  pure $ FuncDef {..}
  where
    funParamP :: Parser (LowerId, Type)
    funParamP = (,) <$> lowerIdP <* lexChar ':' <*> typeP
    instancesP = do
      r <- optional $ lexChar '{'
      case r of
        Just '{' -> sepBy typeP (lexChar ',') <* lexChar '}'
        Nothing -> pure []

classDefP :: Parser ClassDef
classDefP = do
  keyword "class"
  name <- upperIdP
  types <- typeParamsP
  lexChar '{'
  funcs <- many funcDeclP
  lexChar '}'
  pure $ ClassDef name types funcs

instanceDefP :: Parser InstanceDef
instanceDefP = do
  keyword "instance"
  name <- upperIdP
  typeArgs <- typeArgsP
  lexChar '{'
  functions <- many funcDefP
  lexChar '}'
  pure $ InstanceDef {..}

funcDeclP :: Parser FuncDecl
funcDeclP = do
  keyword "fun"
  name <- lowerIdP
  typeParams <- typeParamsP
  lexChar '('
  params <- sepBy typeP (lexChar ',')
  lexChar ')'
  lexChar ':'
  retType <- typeP
  pure $ FuncDecl {..}

typeArgsP :: Parser [Type]
typeArgsP = do
  r <- optional $ lexChar '['
  case r of
    Just '[' -> sepBy typeP (lexChar ',') <* lexChar ']'
    Nothing -> pure []

typeParamsP :: Parser [LowerId]
typeParamsP = do
  r <- optional $ lexChar '['
  case r of
    Just '[' -> sepBy lowerIdP (lexChar ',') <* lexChar ']'
    Nothing -> pure []

-- Type

typeP :: Parser Type
typeP = do
  tp <- baseType
  r <- optional $ lexString "->"
  case r of
    Just "->" -> TypeFunc tp <$> typeP
    Nothing -> pure tp
  where
    baseType :: Parser Type
    baseType = (lexChar '(' >> typeP <* lexChar ')') <|> simpleType
    simpleType :: Parser Type
    simpleType = typeBuiltin <|> typeVar <|> typeData
    typeBuiltin = typeUnit <|> typeBool <|> typeInt <|> typeFloat <|> typeChar <|> typeString
    typeUnit = keyword "Unit" $> TypeUnit
    typeBool = keyword "Bool" $> TypeBool
    typeInt = keyword "Int" $> TypeInt
    typeFloat = keyword "Float" $> TypeFloat
    typeChar = keyword "Char" $> TypeChar
    typeString = keyword "String" $> TypeString
    typeVar = TypeVar <$> lowerIdP
    typeData = do
      name <- upperIdP
      r <- optional $ lexChar '['
      case r of
        Just '[' -> TypeApp name <$> sepBy typeP (lexChar ',') <* lexChar ']'
        Nothing -> pure $ TypeData name

-- Expr

exprP :: Parser Expr
exprP = makeExprParser termP $ fmap binop <$> ops
  where
    binop (str, op) = InfixL $ lexString str $> BinExpr op
    ops =
      [ [("*", MulOp), ("/", DivOp)],
        [("+", AddOp), ("-", SubOp)],
        [("==", EqOp), ("!=", NeqOp), (">=", GteOp), (">", GtOp), ("<=", LteOp), ("<", LtOp)]
      ]

termP :: Parser Expr
termP = do
  expr <- baseExprP
  r <- optional $ lexChar '('
  case r of
    Nothing -> pure expr
    Just '(' -> do
      args <- sepBy exprP (lexChar ',')
      lexChar ')'
      pure $ FuncApp expr args

baseExprP :: Parser Expr
baseExprP = ifExprP <|> matchExprP <|> parenthesesExprP <|> blockExprP <|> literalExprP <|> varExprP

{-HLINT-}
ifExprP :: Parser Expr
ifExprP =
  IfExpr
    <$> (keyword "if" *> exprP)
    <*> (keyword "then" *> exprP)
    <*> (keyword "else" *> exprP)

-- ifExprP = do
--   keyword "if"
--   cond <- exprP
--   keyword "then"
--   ifTrue <- exprP
--   keyword "else"
--   ifFalse <- exprP
--   return $ IfExpr cond ifTrue ifFalse


matchExprP :: Parser Expr
matchExprP = do
  keyword "match"
  expr <- exprP
  lexChar '{'
  cases <- many matchCaseP
  lexChar '}'
  pure $ Match expr cases
  where
    matchCaseP = (,) <$> matchPatternP <* lexString "=>" <*> exprP
    matchPatternP = patLiteralP <|> patNameP <|> patCtorP <|> patHoleP
    patLiteralP = PatLiteral <$> literalP
    patNameP = PatName <$> lowerIdP
    patCtorP = PatCtor <$> upperIdP <* lexChar '(' <*> sepBy matchPatternP (lexChar ',') <* lexChar ')'
    patHoleP = keyword "_" $> PatHole

parenthesesExprP :: Parser Expr
parenthesesExprP = lexChar '(' *> exprP <* lexChar ')'

blockExprP :: Parser Expr
blockExprP = do
  lexChar '{'
  funcs <- many (funcDefP <* lexChar ';')
  elems <- many (try (blockExprElemP <* lexChar ';'))
  expr <- exprP
  lexChar '}'
  pure $ BlockExpr funcs elems expr
  where
    blockExprElemP = blockElemLetP <|> blockElemExprP
    blockElemLetP = keyword "let" >> BlockElemLet <$> lowerIdP <* lexChar '=' <*> exprP
    blockElemExprP = BlockElemExpr <$> exprP

literalExprP :: Parser Expr
literalExprP = ExprLit <$> literalP

varExprP :: Parser Expr
varExprP = ExprVar <$> (unLowerId <$> lowerIdP <|> unUpperId <$> upperIdP)

-- Literals

literalP :: Parser Literal
literalP = unitLitP <|> boolLitP <|> try intLitP <|> floatLitP <|> charLitP <|> stringLitP

unitLitP :: Parser Literal
unitLitP = keyword "unit" $> UnitLit

boolLitP :: Parser Literal
boolLitP = keyword "true" $> BoolLit True <|> (keyword "false" $> BoolLit False)

intLitP :: Parser Literal
intLitP = L.lexeme ws do
  s <- some digitChar
  notFollowedBy (char '.')
  pure $ IntLit $ read s

floatLitP :: Parser Literal
floatLitP = L.lexeme ws do
  s1 <- some digitChar
  char '.'
  s2 <- some digitChar
  pure $ FloatLit $ read (s1 ++ "." ++ s2)

charLitP :: Parser Literal
charLitP = char '\'' >> CharLit <$> basicChar ['\\', '\''] <* lexChar '\''

stringLitP :: Parser Literal
stringLitP = char '"' >> StringLit <$> many (basicChar ['\\', '"']) <* lexChar '"'

basicChar :: [Char] -> Parser Char
basicChar esc = rawChar <|> escapedChar
  where
    rawChar = satisfy (`notElem` esc)
    escapedChar =
      char '\\' >> f <$> oneOf ['\\', '"', '\'', 'n', 't']
      where
        f '\\' = '\\'
        f '\'' = '\''
        f '\"' = '\"'
        f 'n' = '\n'
        f 't' = '\t'

-- Misc

lowerIdP :: Parser LowerId
lowerIdP = L.lexeme ws do
  s <- (:) <$> oneOf lowerLetters <*> many alphaNum
  guard (s `notElem` reservedKeywords)
  pure $ LowerId s

upperIdP :: Parser UpperId
upperIdP = L.lexeme ws do
  s <- (:) <$> oneOf upperLetters <*> many alphaNum
  guard (s `notElem` reservedKeywords)
  pure $ UpperId s

-- Utils

alphaNum :: Parser Char
alphaNum = oneOf (upperLetters ++ lowerLetters ++ digitLetters)

upperLetters :: [Char]
upperLetters = ['A' .. 'Z']

lowerLetters :: [Char]
lowerLetters = ['a' .. 'z']

digitLetters :: [Char]
digitLetters = ['0' .. '9']

keyword :: String -> Parser String
keyword s = L.lexeme ws $ string s <* notFollowedBy alphaNumChar

ws :: Parser ()
ws = L.space space1 (L.skipLineComment "#") empty

lexChar :: Char -> Parser Char
lexChar c = L.lexeme ws $ char c

lexString :: String -> Parser String
lexString s = L.lexeme ws $ string s

reservedKeywords :: [String]
reservedKeywords = reservedLower ++ reservedUpper
  where
    reservedLower = ["data", "fun", "let", "class", "instance", "match", "if", "then", "else", "unit", "true", "false"]
    reservedUpper = ["Unit", "Bool", "Int", "Float", "Char", "String"]

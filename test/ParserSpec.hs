{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import AST
import Control.Exception (evaluate)
import Data.Either (isLeft)
import Data.String (IsString (fromString))
import Data.Void (Void)
import Parser
import Test.Hspec (Expectation, HasCallStack, Spec, describe, it)
import qualified Test.Hspec.Megaparsec as T (shouldParse)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser)

testParse :: Parser a -> String -> Either (ParseErrorBundle String Void) a
testParse p = runParser p "<test input>"

shouldParse :: (HasCallStack, Show a, Eq a) => Parser a -> String -> a -> Expectation
shouldParse p s v = testParse p s `T.shouldParse` v

instance IsString UpperId where
  fromString = UpperId

instance IsString LowerId where
  fromString = LowerId

spec :: Spec
spec = do
  describe "expressions" do
    describe "literals" do
      it "should parse unit literal" do
        exprP `shouldParse` "unit" $
          ExprLit UnitLit

      it "should parse boolean literals" do
        exprP `shouldParse` "true" $
          ExprLit $ BoolLit True

        exprP `shouldParse` "false" $
          ExprLit $ BoolLit False

      it "should parse numeric literals" do
        exprP `shouldParse` "123" $
          ExprLit $ IntLit 123

        exprP `shouldParse` "123.456" $
          ExprLit $ FloatLit 123.456

      it "should parse char literals" do
        exprP `shouldParse` "'a'" $
          ExprLit $ CharLit 'a'

        exprP `shouldParse` "'\\''" $
          ExprLit $ CharLit '\''

        exprP `shouldParse` "'\\\"'" $
          ExprLit $ CharLit '\"'

        exprP `shouldParse` "'\\n'" $
          ExprLit $ CharLit '\n'

      it "should parse string literals" do
        exprP `shouldParse` "\"\"" $
          ExprLit $ StringLit ""

        exprP `shouldParse` "\"abc def\"" $
          ExprLit $ StringLit "abc def"

        exprP `shouldParse` "\"' \\\' \\\"\\t\"" $
          ExprLit $ StringLit "' ' \"\t"

    describe "binary" do
      it "should parse simple binary expressions" do
        exprP `shouldParse` "1 + 2" $
          BinExpr AddOp (ExprLit $ IntLit 1) (ExprLit $ IntLit 2)

        exprP `shouldParse` "x + y" $
          BinExpr AddOp (ExprVar "x") (ExprVar "y")

    describe "match" do
      it "should parse empty match expression" do
        exprP `shouldParse` "match x {}" $
          Match (ExprVar "x") []

  describe "types" do
    it "should parse type variables" do
      typeP `shouldParse` "abc" $
        TypeVar "abc"

    it "should parse type data" do
      typeP `shouldParse` "Abc" $
        TypeData "Abc"

    it "should parse Unit type" do
      typeP `shouldParse` "Unit" $
        TypeUnit

    it "should parse function type" do
      typeP `shouldParse` "a -> b" $
        TypeFunc (TypeVar "a") (TypeVar "b")

    it "should parse nested function type" do
      typeP `shouldParse` "a -> b -> c" $
        TypeFunc (TypeVar "a") (TypeFunc (TypeVar "b") (TypeVar "c"))

      typeP `shouldParse` "(a -> b) -> c" $
        TypeFunc (TypeFunc (TypeVar "a") (TypeVar "b")) (TypeVar "c")

    it "should parse type application" do
      typeP `shouldParse` "Abc[x]" $
        TypeApp "Abc" [TypeVar "x"]

      typeP `shouldParse` "Abc[x, y]" $
        TypeApp "Abc" [TypeVar "x", TypeVar "y"]

    it "should parse nested type application" do
      typeP `shouldParse` "Abc[Xyz[d], e -> f]" $
        TypeApp "Abc" [TypeApp "Xyz" [TypeVar "d"], TypeFunc (TypeVar "e") (TypeVar "f")]

  describe "statements" do
    describe "data definitions" do
      it "should parse trivial data definitions" do
        statementP `shouldParse` "data Test { }" $
          DataStmt $ DataDef "Test" [] []

        statementP `shouldParse` "data Test{}" $
          DataStmt $ DataDef "Test" [] []

      it "should parse data definitions with type parameters" do
        statementP `shouldParse` "data Test[a]{}" $
          DataStmt $ DataDef "Test" ["a"] []

        statementP `shouldParse` "data Test [ a , b ] { }" $
          DataStmt $ DataDef "Test" ["a", "b"] []

      it "should parse data definitions with empty constructors" do
        statementP `shouldParse` "data List { Nil() Cons() }" $
          DataStmt $ DataDef "List" [] [("Nil", []), ("Cons", [])]

        statementP `shouldParse` "data Test [a, b] { Nil() Cons() }" $
          DataStmt $ DataDef "Test" ["a", "b"] [("Nil", []), ("Cons", [])]

      it "should parse data definitions with constructors with types" do
        statementP `shouldParse` "data List[a] { Nil() Cons(a, List[a]) }" $
          DataStmt $ DataDef "List" ["a"] [("Nil", []), ("Cons", [TypeVar "a", TypeApp "List" [TypeVar "a"]])]

        statementP `shouldParse` "data Pair[a, b] { Pair(a, b) }" $
          DataStmt $ DataDef "Pair" ["a", "b"] [("Pair", [TypeVar "a", TypeVar "b"])]

    describe "function definitions" do
      it "should parse simple function definition" do
        statementP `shouldParse` "fun test1(): Unit = unit" $
          FuncStmt $ FuncDef "test1" [] [] [] TypeUnit (ExprLit UnitLit)

      it "should parse function definition with arguments" do
        statementP `shouldParse` "fun add(x: Int, y: Int): Int = x + y" $
          FuncStmt $ FuncDef "add" [] [] [("x", TypeInt), ("y", TypeInt)] TypeInt (BinExpr AddOp (ExprVar "x") (ExprVar "y"))

-- it "returns the first element of an *arbitrary* list" $
--   property $ \x xs -> head (x : xs) == (x :: Int)

-- it "throws an exception if used with an empty list" $ do
--   evaluate (head []) `shouldThrow` anyException

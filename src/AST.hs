{-# LANGUAGE DuplicateRecordFields #-}

module AST where

import Data.List.NonEmpty (NonEmpty)

-- Statements

data Statement
  = DataStmt DataDef
  | FuncStmt FuncDef
  | ClassStmt ClassDef
  | InstanceStmt InstanceDef
  deriving (Show, Eq)

data DataDef = DataDef
  { name :: UpperId,
    typeParams :: [LowerId],
    ctors :: [(UpperId, [Type])]
  }
  deriving (Show, Eq)

data FuncDef = FuncDef
  { name :: LowerId,
    typeParams :: [LowerId],
    instances :: [Type],
    params :: [(LowerId, Type)],
    retType :: Type,
    expr :: Expr
  }
  deriving (Show, Eq)

data ClassDef = ClassDef
  { name :: UpperId,
    typeParams :: [LowerId],
    functions :: [FuncDecl]
  }
  deriving (Show, Eq)

data InstanceDef = InstanceDef
  { name :: UpperId,
    typeArgs :: [Type],
    functions :: [FuncDef]
  }
  deriving (Show, Eq)

data FuncDecl = FuncDecl
  { name :: LowerId,
    typeParams :: [LowerId],
    params :: [Type],
    retType :: Type
  }
  deriving (Show, Eq)

-- Types

data Type
  = TypeUnit
  | TypeBool
  | TypeInt
  | TypeFloat
  | TypeChar
  | TypeString
  | TypeFunc Type Type
  | TypeVar LowerId
  | TypeData UpperId
  | TypeApp UpperId [Type]
  -- separation betweeen TypeData and TypeApp might be useless - investigate and refactor
  deriving (Show, Eq)

-- Expressions

data Expr
  = ExprLit Literal
  | ExprVar String
  | BinExpr BinOp Expr Expr
  | FuncApp Expr [Expr]
  | IfExpr Expr Expr Expr
  | Match Expr [(Pattern, Expr)]
  | BlockExpr [FuncDef] [BlockElem] Expr
  deriving (Show, Eq)

data BlockElem
  = BlockElemLet LowerId Expr
  | BlockElemExpr Expr
  deriving (Show, Eq)

data Literal
  = UnitLit
  | BoolLit Bool
  | IntLit Integer
  | FloatLit Double
  | CharLit Char
  | StringLit String
  deriving (Show, Eq)

data BinOp
  = DivOp
  | MulOp
  | AddOp
  | SubOp
  | EqOp
  | NeqOp
  | GteOp
  | GtOp
  | LteOp
  | LtOp
  deriving (Show, Eq)

data Pattern
  = PatLiteral Literal
  | PatName LowerId
  | PatCtor UpperId [Pattern]
  | PatHole
  deriving (Show, Eq)

-- Common

newtype LowerId = LowerId {unLowerId :: String} deriving (Show, Eq)

newtype UpperId = UpperId {unUpperId :: String} deriving (Show, Eq)

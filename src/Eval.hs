{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Eval where

import AST
import Control.Monad (foldM, (>=>))
import Data.Functor (($>))
import Data.List (find)
import qualified Data.Map.Strict as Map
import Utils (foldM2)

instance Show (Value -> IO Value) where
  show s = "<function>"

instance Eq (Value -> IO Value) where
  a == b = False

data Value
  = UnitVal
  | BoolVal Bool
  | IntVal Integer
  | FloatVal Double
  | ChrVal Char
  | StrVal String
  | DataVal String [Value]
  | FuncVal (Value -> IO Value)
  deriving (Show, Eq)

type Env = Map.Map String Value

initialEnv :: Env
initialEnv = foldl (\e (s, v) -> envAdd s v e) Map.empty vars
  where
    vars =
      [ ("printStr", printStr),
        ("printStrLn", printStrLn),
        ("printInt", printInt),
        ("printFloat", printFloat),
        ("readStr", readStr),
        ("readInt", readInt),
        ("debug", debug)
      ]
    printStr = FuncVal $ \(StrVal s) -> putStr s $> UnitVal
    printStrLn = FuncVal $ \(StrVal s) -> putStrLn s $> UnitVal
    printInt = FuncVal $ \(IntVal i) -> putStr (show i) $> UnitVal
    printFloat = FuncVal $ \(FloatVal f) -> putStr (show f) $> UnitVal
    readStr = FuncVal $ \UnitVal -> StrVal <$> getLine
    readInt = FuncVal $ \UnitVal -> IntVal <$> readLn
    debug = FuncVal $ \v -> print v $> UnitVal

envLookup :: Env -> String -> Value
envLookup e s = case Map.lookup s e of
  Just v -> v
  Nothing -> error $ "Env error: variable " ++ s ++ " not in scope!"

envAdd :: String -> Value -> Env -> Env
envAdd = Map.insert

evalProgram :: [Statement] -> IO ()
evalProgram prog =
  case mainFn of
    Just _ -> evalExpr globalEnv (FuncApp (ExprVar "main") [ExprLit UnitLit]) $> ()
    Nothing -> putStrLn "Error: main function not found"
  where
    mainFn = find isMainFn prog
    isMainFn (FuncStmt (FuncDef {name = (LowerId "main")})) = True
    isMainFn _ = False
    globalEnv = foldl addStmt initialEnv prog
    addStmt env (FuncStmt fd) = addFuncToEnv globalEnv env fd
    addStmt env (DataStmt (DataDef {ctors})) = foldl addCtor env ctors
    addStmt env (ClassStmt _) = error "CLASSES ARE NOT YET SUPPORTED"
    addStmt env (InstanceStmt _) = error "INSTANCES ARE NOT YET SUPPORTED"
    addCtor :: Env -> (UpperId, [Type]) -> Env
    addCtor env (UpperId name, xs) =
      envAdd name (fnVal (length xs) []) env
      where
        fnVal 0 _ = FuncVal $ \UnitVal -> pure $ DataVal name []
        fnVal 1 vals = FuncVal $ \v -> pure $ DataVal name $ vals ++ [v]
        fnVal n vals = FuncVal $ \v -> pure $ fnVal (n - 1) $ vals ++ [v]

evalLit :: Literal -> Value
evalLit UnitLit = UnitVal
evalLit (BoolLit b) = BoolVal b
evalLit (IntLit i) = IntVal i
evalLit (FloatLit f) = FloatVal f
evalLit (CharLit c) = ChrVal c
evalLit (StringLit s) = StrVal s

evalExpr :: Env -> Expr -> IO Value
evalExpr env (ExprLit lit) = pure $ evalLit lit
evalExpr env (ExprVar name) = pure $ envLookup env name
evalExpr env (BinExpr op e1 e2) = do
  v1 <- evalExpr env e1
  v2 <- evalExpr env e2
  let calc = case op of
        DivOp -> \v1 v2 -> case (v1, v2) of
          (IntVal i1, IntVal i2) -> IntVal $ div i1 i2
          (FloatVal f1, FloatVal f2) -> FloatVal $ f1 / f2
          (_, _) -> error "Error: attempt to use binary operator on incompatible types"
        MulOp -> evalNum (*)
        AddOp -> evalNum (+)
        SubOp -> evalNum (-)
        EqOp -> evalEq
        NeqOp -> evalNeq
        GteOp -> evalNumBool (>=)
        GtOp -> evalNumBool (>)
        LteOp -> evalNumBool (<=)
        LtOp -> evalNumBool (<)
  pure $ calc v1 v2
  where
    evalNum :: (forall a. Num a => a -> a -> a) -> Value -> Value -> Value
    evalNum fn (IntVal i1) (IntVal i2) = IntVal $ fn i1 i2
    evalNum fn (FloatVal f1) (FloatVal f2) = FloatVal $ fn f1 f2
    evalNum _ _ _ = error "Error: attempt to use binary operator on incompatible types"
    evalNumBool :: (forall a. Ord a => a -> a -> Bool) -> Value -> Value -> Value
    evalNumBool fn (IntVal i1) (IntVal i2) = BoolVal $ fn i1 i2
    evalNumBool fn (FloatVal f1) (FloatVal f2) = BoolVal $ fn f1 f2
    evalNumBool _ _ _ = error "Error: attempt to use comparison operator on incompatible types"
    evalEq a b = BoolVal $ checkEq a b
    evalNeq a b = BoolVal $ not $ checkEq a b
    checkEq UnitVal UnitVal = True
    checkEq (BoolVal b1) (BoolVal b2) = b1 == b2
    checkEq (IntVal i1) (IntVal i2) = i1 == i2
    checkEq (FloatVal f1) (FloatVal f2) = f1 == f2
    checkEq (ChrVal c1) (ChrVal c2) = c1 == c2
    checkEq (StrVal s1) (StrVal s2) = s1 == s2
    checkEq (DataVal s1 vs1) (DataVal s2 vs2) = s1 == s2 && all (uncurry checkEq) (zip vs1 vs2)
    checkEq (FuncVal _) _ = False
    checkEq _ (FuncVal _) = False
    checkEq _ _ = False
evalExpr env (FuncApp e []) = do
  (FuncVal fn) <- evalExpr env e
  fn UnitVal
evalExpr env (FuncApp e es) = do
  fnVal <- evalExpr env e
  foldM callFn fnVal es
  where
    callFn (FuncVal fn) = evalExpr env >=> fn
evalExpr env (IfExpr cond ifTrue ifFalse) =
  evalExpr env cond >>= \case
    BoolVal True -> evalExpr env ifTrue
    BoolVal False -> evalExpr env ifFalse
evalExpr env (Match expr pats) = do
  v <- evalExpr env expr
  findPat v pats
  where
    findPat :: Value -> [(Pattern, Expr)] -> IO Value
    findPat v [] = error "Error: non exhaustive pattern match"
    findPat v ((p, expr) : ps) =
      case checkMatch env v p of
        Just newEnv -> evalExpr newEnv expr
        Nothing -> findPat v ps
    checkMatch :: Env -> Value -> Pattern -> Maybe Env
    checkMatch e v (PatLiteral (evalLit -> litVal)) = if v == litVal then Just env else Nothing
    checkMatch e v (PatName (LowerId name)) = Just $ envAdd name v e
    checkMatch e (DataVal ctor vs) (PatCtor (UpperId name) ps) =
      if ctor == name
        then foldM2 checkMatch e vs ps
        else Nothing
    checkMatch e _ PatHole = Just e
    checkMatch _ _ _ = Nothing
evalExpr env (BlockExpr fs bs expr) = do
  localEnv <- localEnvIO
  evalExpr localEnv expr
  where
    localFnEnv = foldl (addFuncToEnv localFnEnv) env fs
    localEnvIO = foldM execElem localFnEnv bs
    execElem e (BlockElemLet (LowerId name) expr) = do
      v <- evalExpr e expr
      pure $ envAdd name v e
    execElem e (BlockElemExpr expr) = evalExpr e expr $> e

addFuncToEnv :: Env -> Env -> FuncDef -> Env
addFuncToEnv localEnv env (FuncDef {name = (LowerId strName), params, expr}) =
  envAdd strName (buildFnVal localEnv $ map (unLowerId . fst) params) env
  where
    buildFnVal _ [] = FuncVal $ \UnitVal -> evalExpr localEnv expr
    buildFnVal env [x] = FuncVal $ \v -> evalExpr (envAdd x v env) expr
    buildFnVal env (x : xs) = FuncVal $ \v -> pure $ buildFnVal (envAdd x v env) xs

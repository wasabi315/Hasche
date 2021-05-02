{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Hasche.Builtins.Primitives where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Cont hiding (cont)
import Control.Monad.Reader
import Data.Foldable
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Hasche.Eval
import Hasche.Format
import Hasche.Object
import Hasche.Reader
import Hasche.SExpr
import System.Exit
import System.IO

-- primitive functions

primEval :: (MonadIO m, MonadEval n) => m (ObjRef n)
primEval =
  mkPrim1 \_ o -> do
    me <- toSExpr o
    case me of
      Nothing -> throw (EvalError "Failed to convert object to expression")
      Just e -> ask >>= flip eval e

primApply :: (MonadIO m, MonadEval n) => m (ObjRef n)
primApply =
  prim \env -> \case
    [] -> throw (EvalError "Arity Mismatch")
    [_] -> throw (EvalError "Arity Mismatch")
    f : xs -> do
      args <- (init xs ++) <$!> pairToList (last xs)
      apply env f args
  where
    pairToList o =
      deref o >>= \case
        Empty -> pure []
        Cons o1 o2 -> do
          os <- pairToList o2
          pure $! (o1 : os)
        _ -> pure [o]

primIsNull :: (MonadIO m, MonadEval n) => m (ObjRef n)
primIsNull =
  mkPrim1 . const $
    deref >=> \case
      Empty -> pure true
      _ -> pure false

primIsBool :: (MonadIO m, MonadEval n) => m (ObjRef n)
primIsBool =
  mkPrim1 . const $
    deref >=> \case
      Bool _ -> pure true
      _ -> pure false

primIsNum :: (MonadIO m, MonadEval n) => m (ObjRef n)
primIsNum =
  mkPrim1 . const $
    deref >=> \case
      Num _ -> pure true
      _ -> pure false

primIsStr :: (MonadIO m, MonadEval n) => m (ObjRef n)
primIsStr =
  mkPrim1 . const $
    deref >=> \case
      Str _ -> pure true
      _ -> pure false

primIsSym :: (MonadIO m, MonadEval n) => m (ObjRef n)
primIsSym =
  mkPrim1 . const $
    deref >=> \case
      Sym _ -> pure true
      _ -> pure false

primIsPair :: (MonadIO m, MonadEval n) => m (ObjRef n)
primIsPair =
  mkPrim1 . const $
    deref >=> \case
      Cons _ _ -> pure true
      _ -> pure false

primIsProc :: (MonadIO m, MonadEval n) => m (ObjRef n)
primIsProc =
  mkPrim1 . const $
    deref >=> \case
      Prim _ -> pure true
      Func _ _ -> pure true
      _ -> pure false

primAdd :: (MonadIO m, MonadEval n) => m (ObjRef n)
primAdd = mkNumFoldPrim (+) 0

primMul :: (MonadIO m, MonadEval n) => m (ObjRef n)
primMul = mkNumFoldPrim (*) 1

primSub :: (MonadIO m, MonadEval n) => m (ObjRef n)
primSub =
  prim . const $
    traverse expectNum >=> \case
      [] -> throw (EvalError "expect at least one number")
      n : ns -> num $! n - sum ns

primDiv :: (MonadIO m, MonadEval n) => m (ObjRef n)
primDiv =
  prim . const $
    traverse expectNum >=> \case
      [] -> throw (EvalError "expect at least one number")
      n : ns -> num $! n - product ns

primNumEq :: (MonadIO m, MonadEval n) => m (ObjRef n)
primNumEq = mkNumBinPred (==)

primGt :: (MonadIO m, MonadEval n) => m (ObjRef n)
primGt = mkNumBinPred (>)

primGe :: (MonadIO m, MonadEval n) => m (ObjRef n)
primGe = mkNumBinPred (>=)

primLt :: (MonadIO m, MonadEval n) => m (ObjRef n)
primLt = mkNumBinPred (<)

primLe :: (MonadIO m, MonadEval n) => m (ObjRef n)
primLe = mkNumBinPred (<=)

primEq, primEqv, primEqual :: (MonadIO m, MonadEval n) => m (ObjRef n)
primEq = mkPrim2 . const $ eq
primEqv = mkPrim2 . const $ eqv
primEqual = mkPrim2 . const $ equal

eq :: MonadIO m => ObjRef n -> ObjRef n -> m (ObjRef n)
eq x y = pure if x == y then true else false

eqv :: MonadIO m => ObjRef n -> ObjRef n -> m (ObjRef n)
eqv x y = do
  v <- deref x
  w <- deref y
  case (v, w) of
    (Num n, Num m) -> pure if n == m then true else false
    _ -> eq x y

equal :: MonadIO m => ObjRef n -> ObjRef n -> m (ObjRef n)
equal x y = do
  v <- deref x
  w <- deref y
  case (v, w) of
    (Cons a b, Cons c d) -> do
      t <- equal a c
      if t == true then equal b d else pure false
    _ -> eqv x y

primStrAppend :: (MonadIO m, MonadEval n) => m (ObjRef n)
primStrAppend =
  prim . const $
    traverse expectStr >=> \ts -> str $! T.concat ts

primStrNum :: (MonadIO m, MonadEval n) => m (ObjRef n)
primStrNum =
  mkPrim1 . const $
    expectStr >=> \s -> case readSNum s of
      Just (SNum n) -> num n
      _ -> throw (EvalError "Failed to convert string to number")

primNumStr :: (MonadIO m, MonadEval n) => m (ObjRef n)
primNumStr =
  mkPrim1 . const $
    expectNum >=> str . T.pack . show

primStrSym :: (MonadIO m, MonadEval n) => m (ObjRef n)
primStrSym =
  mkPrim1 . const $
    expectStr >=> sym

primSymStr :: (MonadIO m, MonadEval n) => m (ObjRef n)
primSymStr =
  mkPrim1 . const $
    expectSym >=> str

primCons :: (MonadIO m, MonadEval n) => m (ObjRef n)
primCons = mkPrim2 \_ x y -> cons x y

primCar :: (MonadIO m, MonadEval n) => m (ObjRef n)
primCar = mkPrim1 \_ x -> fst <$!> expectCons x

primCdr :: (MonadIO m, MonadEval n) => m (ObjRef n)
primCdr = mkPrim1 \_ x -> snd <$!> expectCons x

primSetCar :: (MonadIO m, MonadEval n) => m (ObjRef n)
primSetCar = mkPrim2 \_ x y -> do
  (car, _) <- expectCons x
  v <- deref y
  undef <$ (car .= v)

primSetCdr :: (MonadIO m, MonadEval n) => m (ObjRef n)
primSetCdr = mkPrim2 \_ x y -> do
  (_, cdr) <- expectCons x
  v <- deref y
  undef <$ (cdr .= v)

primCallCC :: (MonadIO m, MonadEval n) => m (ObjRef n)
primCallCC =
  mkPrim1 \env o ->
    callCC \k -> do
      c <- cont k
      apply env o [c]

primOpenInputFile :: (MonadIO m, MonadEval n) => m (ObjRef n)
primOpenInputFile = mkFileOpenPrim ReadMode

primOpenOutputFile :: (MonadIO m, MonadEval n) => m (ObjRef n)
primOpenOutputFile = mkFileOpenPrim WriteMode

primCloseInputPort :: (MonadIO m, MonadEval n) => m (ObjRef n)
primCloseInputPort =
  mkPrim1 \_ o -> do
    h <- expectPort o
    undef <$ liftIO (hClose h)

primCloseOutputPort :: (MonadIO m, MonadEval n) => m (ObjRef n)
primCloseOutputPort = primCloseInputPort

primRead :: (MonadIO m, MonadEval n) => m (ObjRef n)
primRead =
  prim . const $ \os -> do
    h <- case os of
      [] -> pure stdin
      [o] -> expectPort o
      _ -> throw (EvalError "Arity Mismatch")
    txt <- liftIO (T.hGetContents h)
    case readSExprList "" txt of
      Left err -> throw (ReadError err)
      Right es -> foldrM (\e o -> fromSExpr e >>= flip cons o) empty es

primDisplay :: (MonadIO m, MonadEval n) => m (ObjRef n)
primDisplay =
  prim . const $ \os -> do
    (o, h) <- case os of
      [o] -> pure (o, stdout)
      [o1, o2] -> (o1,) <$> expectPort o2
      _ -> throw (EvalError "ArityMismatch")
    undef <$ (display o >>= liftIO . T.hPutStr h)

primWrite :: (MonadIO m, MonadEval n) => m (ObjRef n)
primWrite =
  prim . const $ \os -> do
    (o, h) <- case os of
      [o] -> pure (o, stdout)
      [o1, o2] -> (o1,) <$> expectPort o2
      _ -> throw (EvalError "ArityMismatch")
    undef <$ (write o >>= liftIO . T.hPutStr h)

primExit :: (MonadIO m, MonadEval n) => m (ObjRef n)
primExit =
  mkPrim0 \_ -> undef <$ liftIO exitSuccess

-- smart constructors

mkPrim0 :: (MonadIO m, MonadEval n) => (Env n -> n (ObjRef n)) -> m (ObjRef n)
mkPrim0 f =
  prim \env os ->
    case os of
      [] -> f env
      _ -> throw (EvalError "Arity Mismatch")

mkPrim1 :: (MonadIO m, MonadEval n) => (Env n -> ObjRef n -> n (ObjRef n)) -> m (ObjRef n)
mkPrim1 f =
  prim \env os ->
    case os of
      [o] -> f env o
      _ -> throw (EvalError "Arity Mismatch")

mkPrim2 :: (MonadIO m, MonadEval n) => (Env n -> ObjRef n -> ObjRef n -> n (ObjRef n)) -> m (ObjRef n)
mkPrim2 f =
  prim \env os ->
    case os of
      [o1, o2] -> f env o1 o2
      _ -> throw (EvalError "Arity Mismatch")

mkNumFoldPrim :: (MonadIO m, MonadEval n) => (Integer -> Integer -> Integer) -> Integer -> m (ObjRef n)
mkNumFoldPrim f z =
  prim \_ os -> do
    ns <- traverse expectNum os
    num $! foldl' f z ns

mkNumBinPred :: (MonadIO m, MonadEval n) => (Integer -> Integer -> Bool) -> m (ObjRef n)
mkNumBinPred p =
  mkPrim2 \_ o1 o2 -> do
    n1 <- expectNum o1
    n2 <- expectNum o2
    pure $! if p n1 n2 then true else false

mkFileOpenPrim :: (MonadIO m, MonadEval n) => IOMode -> m (ObjRef n)
mkFileOpenPrim mode =
  mkPrim1 \_ o -> do
    path <- expectStr o
    h <- liftIO $ openFile (T.unpack path) mode
    port h

-- Value extraction

expectNum :: (MonadIO m, MonadThrow m) => ObjRef n -> m Integer
expectNum obj =
  deref obj >>= \case
    Num n -> pure n
    _ -> throw (EvalError "expect number")

expectStr :: (MonadIO m, MonadThrow m) => ObjRef n -> m Text
expectStr obj =
  deref obj >>= \case
    Str s -> pure s
    _ -> throw (EvalError "expect number")

expectSym :: (MonadIO m, MonadThrow m) => ObjRef n -> m Text
expectSym obj =
  deref obj >>= \case
    Sym s -> pure s
    _ -> throw (EvalError "expect number")

expectPort :: (MonadIO m, MonadThrow m) => ObjRef n -> m Handle
expectPort obj =
  deref obj >>= \case
    Port h -> pure h
    _ -> throw (EvalError "expect number")

expectCons :: (MonadIO m, MonadThrow m) => ObjRef n -> m (ObjRef n, ObjRef n)
expectCons obj =
  deref obj >>= \case
    Cons car cdr -> pure (car, cdr)
    _ -> throw (EvalError "expect cons")

-- application

apply :: MonadEval m => Env m -> ObjRef m -> [ObjRef m] -> m (ObjRef m)
apply env x xs = do
  deref x >>= \case
    Prim f -> do
      f env xs
    Func env' f -> do
      f env' xs
    Cont k -> do
      case xs of
        [arg] -> k arg
        _ -> throw (EvalError "Arity mismatch")
    _ -> throw (EvalError "Could not apply")

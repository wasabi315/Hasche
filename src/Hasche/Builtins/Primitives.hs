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

primEval :: (MonadIO m, MonadEval n) => m (Object n)
primEval =
  mkPrim1 \o -> do
    me <- toSExpr o
    case me of
      Nothing -> throw (EvalError "Failed to convert object to expression")
      Just e -> ask >>= flip eval e

primApply :: (MonadIO m, MonadEval n) => m (Object n)
primApply =
  prim \case
    [] -> throw (EvalError "Arity Mismatch")
    [_] -> throw (EvalError "Arity Mismatch")
    f : xs -> do
      args <- (init xs ++) <$!> pairToList (last xs)
      apply f args
  where
    pairToList o = case o of
      Empty -> pure []
      Cons car cdr -> do
        o1 <- deref car
        o2 <- deref cdr
        os <- pairToList o2
        pure $! (o1 : os)
      _ -> pure [o]

primIsNull :: (MonadIO m, MonadEval n) => m (Object n)
primIsNull =
  mkPrim1 \case
    Empty -> pure true
    _ -> pure false

primIsBool :: (MonadIO m, MonadEval n) => m (Object n)
primIsBool =
  mkPrim1 \case
    Bool _ -> pure true
    _ -> pure false

primIsNum :: (MonadIO m, MonadEval n) => m (Object n)
primIsNum =
  mkPrim1 \case
    Num _ -> pure true
    _ -> pure false

primIsStr :: (MonadIO m, MonadEval n) => m (Object n)
primIsStr =
  mkPrim1 \case
    Str _ -> pure true
    _ -> pure false

primIsSym :: (MonadIO m, MonadEval n) => m (Object n)
primIsSym =
  mkPrim1 \case
    Sym _ -> pure true
    _ -> pure false

primIsPair :: (MonadIO m, MonadEval n) => m (Object n)
primIsPair =
  mkPrim1 \case
    Cons _ _ -> pure true
    _ -> pure false

primIsProc :: (MonadIO m, MonadEval n) => m (Object n)
primIsProc =
  mkPrim1 \case
    Prim _ -> pure true
    Func _ _ -> pure true
    _ -> pure false

primAdd :: (MonadIO m, MonadEval n) => m (Object n)
primAdd = mkNumFoldPrim (+) 0

primMul :: (MonadIO m, MonadEval n) => m (Object n)
primMul = mkNumFoldPrim (*) 1

primSub :: (MonadIO m, MonadEval n) => m (Object n)
primSub =
  prim $
    traverse expectNum >=> \case
      [] -> throw (EvalError "expect at least one number")
      n : ns -> num $! n - sum ns

primDiv :: (MonadIO m, MonadEval n) => m (Object n)
primDiv =
  prim $
    traverse expectNum >=> \case
      [] -> throw (EvalError "expect at least one number")
      n : ns -> num $! n - product ns

primNumEq :: (MonadIO m, MonadEval n) => m (Object n)
primNumEq = mkNumBinPred (==)

primGt :: (MonadIO m, MonadEval n) => m (Object n)
primGt = mkNumBinPred (>)

primGe :: (MonadIO m, MonadEval n) => m (Object n)
primGe = mkNumBinPred (>=)

primLt :: (MonadIO m, MonadEval n) => m (Object n)
primLt = mkNumBinPred (<)

primLe :: (MonadIO m, MonadEval n) => m (Object n)
primLe = mkNumBinPred (<=)

primEq, primEqv, primEqual :: (MonadIO m, MonadEval n) => m (Object n)
primEq = mkPrim2 eq
primEqv = mkPrim2 eqv
primEqual = mkPrim2 equal

eq :: MonadIO m => Object n -> Object n -> m (Object n)
eq x y = pure if x == y then true else false

eqv :: MonadIO m => Object n -> Object n -> m (Object n)
eqv (Num n) (Num m) = pure if n == m then true else false
eqv x y = eq x y

equal :: MonadIO m => Object n -> Object n -> m (Object n)
equal (Cons r1 r2) (Cons r3 r4) = do
  o1 <- deref r1
  o3 <- deref r3
  t <- equal o1 o3
  if t == false
    then pure false
    else do
      o2 <- deref r2
      o4 <- deref r4
      equal o2 o4
equal x y = eqv x y

primStrAppend :: (MonadIO m, MonadEval n) => m (Object n)
primStrAppend =
  prim $
    traverse expectStr >=> \ts -> str $! T.concat ts

primStrNum :: (MonadIO m, MonadEval n) => m (Object n)
primStrNum =
  mkPrim1 $
    expectStr >=> \s -> case readSNum s of
      Just (SNum n) -> num n
      _ -> throw (EvalError "Failed to convert string to number")

primNumStr :: (MonadIO m, MonadEval n) => m (Object n)
primNumStr = mkPrim1 $ expectNum >=> str . T.pack . show

primStrSym :: (MonadIO m, MonadEval n) => m (Object n)
primStrSym = mkPrim1 $ expectStr >=> sym

primSymStr :: (MonadIO m, MonadEval n) => m (Object n)
primSymStr = mkPrim1 $ expectSym >=> str

primGensym :: (MonadIO m, MonadEval n) => m (Object n)
primGensym = mkPrim0 gensym

primCons :: (MonadIO m, MonadEval n) => m (Object n)
primCons = mkPrim2 cons

primCar :: (MonadIO m, MonadEval n) => m (Object n)
primCar = mkPrim1 $ expectCons >=> deref . fst

primCdr :: (MonadIO m, MonadEval n) => m (Object n)
primCdr = mkPrim1 $ expectCons >=> deref . snd

primSetCar :: (MonadIO m, MonadEval n) => m (Object n)
primSetCar =
  mkPrim2 \x y -> do
    (car, _) <- expectCons x
    undef <$ (car .= y)

primSetCdr :: (MonadIO m, MonadEval n) => m (Object n)
primSetCdr =
  mkPrim2 \x y -> do
    (_, cdr) <- expectCons x
    undef <$ (cdr .= y)

primCallCC :: (MonadIO m, MonadEval n) => m (Object n)
primCallCC =
  mkPrim1 \o ->
    callCC \k -> do
      c <- cont k
      apply o [c]

primOpenInputFile :: (MonadIO m, MonadEval n) => m (Object n)
primOpenInputFile = mkFileOpenPrim ReadMode

primOpenOutputFile :: (MonadIO m, MonadEval n) => m (Object n)
primOpenOutputFile = mkFileOpenPrim WriteMode

primCloseInputPort :: (MonadIO m, MonadEval n) => m (Object n)
primCloseInputPort =
  mkPrim1 \o -> do
    h <- expectPort o
    undef <$ liftIO (hClose h)

primCloseOutputPort :: (MonadIO m, MonadEval n) => m (Object n)
primCloseOutputPort = primCloseInputPort

primRead :: (MonadIO m, MonadEval n) => m (Object n)
primRead =
  prim \os -> do
    h <- case os of
      [] -> pure stdin
      [o] -> expectPort o
      _ -> throw (EvalError "Arity Mismatch")
    txt <- liftIO (T.hGetContents h)
    case readSExprList "" txt of
      Left e -> throw (ReadError e)
      Right es -> foldrM (\e o -> fromSExpr e >>= flip cons o) empty es

primDisplay :: (MonadIO m, MonadEval n) => m (Object n)
primDisplay =
  prim \os -> do
    (o, h) <- case os of
      [o] -> pure (o, stdout)
      [o1, o2] -> (o1,) <$> expectPort o2
      _ -> throw (EvalError "ArityMismatch")
    undef <$ (display o >>= liftIO . T.hPutStr h)

primWrite :: (MonadIO m, MonadEval n) => m (Object n)
primWrite =
  prim \os -> do
    (o, h) <- case os of
      [o] -> pure (o, stdout)
      [o1, o2] -> (o1,) <$> expectPort o2
      _ -> throw (EvalError "ArityMismatch")
    undef <$ (write o >>= liftIO . T.hPutStr h)

primExit :: (MonadIO m, MonadEval n) => m (Object n)
primExit = mkPrim0 $ undef <$ throw ExitSuccess

primError :: (MonadIO m, MonadEval n) => m (Object n)
primError = prim $ traverse display >=> throw . UserError . T.concat

-- smart constructors

mkPrim0 :: (MonadIO m, MonadEval n) => n (Object n) -> m (Object n)
mkPrim0 x =
  prim \case
    [] -> x
    _ -> throw (EvalError "Arity Mismatch")

mkPrim1 :: (MonadIO m, MonadEval n) => (Object n -> n (Object n)) -> m (Object n)
mkPrim1 f =
  prim \case
    [o] -> f o
    _ -> throw (EvalError "Arity Mismatch")

mkPrim2 :: (MonadIO m, MonadEval n) => (Object n -> Object n -> n (Object n)) -> m (Object n)
mkPrim2 f =
  prim \case
    [o1, o2] -> f o1 o2
    _ -> throw (EvalError "Arity Mismatch")

mkNumFoldPrim :: (MonadIO m, MonadEval n) => (Integer -> Integer -> Integer) -> Integer -> m (Object n)
mkNumFoldPrim f z =
  prim \os -> do
    ns <- traverse expectNum os
    num $! foldl' f z ns

mkNumBinPred :: (MonadIO m, MonadEval n) => (Integer -> Integer -> Bool) -> m (Object n)
mkNumBinPred p =
  mkPrim2 \o1 o2 -> do
    n1 <- expectNum o1
    n2 <- expectNum o2
    pure $! if p n1 n2 then true else false

mkFileOpenPrim :: (MonadIO m, MonadEval n) => IOMode -> m (Object n)
mkFileOpenPrim mode =
  mkPrim1 \o -> do
    path <- expectStr o
    h <- liftIO $ openFile (T.unpack path) mode `catchIO` (throw . FileError)
    port h

-- Value extraction

expectNum :: (MonadIO m, MonadThrow m) => Object n -> m Integer
expectNum = \case
  Num n -> pure n
  _ -> throw (EvalError "expect number")

expectStr :: (MonadIO m, MonadThrow m) => Object n -> m Text
expectStr = \case
  Str s -> pure s
  _ -> throw (EvalError "expect number")

expectSym :: (MonadIO m, MonadThrow m) => Object n -> m Text
expectSym = \case
  Sym s -> pure s
  _ -> throw (EvalError "expect number")

expectPort :: (MonadIO m, MonadThrow m) => Object n -> m Handle
expectPort = \case
  Port h -> pure h
  _ -> throw (EvalError "expect number")

expectCons :: (MonadIO m, MonadThrow m) => Object n -> m (ObjRef n, ObjRef n)
expectCons = \case
  Cons car cdr -> pure (car, cdr)
  _ -> throw (EvalError "expect cons")

-- application

apply :: MonadEval m => Object m -> [Object m] -> m (Object m)
apply x xs = do
  case x of
    Prim f -> f xs
    Func env' f -> f env' xs
    Cont k ->
      case xs of
        [arg] -> k arg
        _ -> throw (EvalError "Arity mismatch")
    _ -> throw (EvalError "Could not apply")

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
import Hasche.Cell
import Hasche.Eval
import Hasche.Format
import Hasche.Object
import Hasche.Reader
import Hasche.SExpr
import System.Exit
import System.IO

-- funcitive functions

funcEval :: (MonadIO m, MonadEval n) => m (Object n)
funcEval =
  mkFunc1 \o -> do
    me <- toSExpr o
    case me of
      Nothing -> throw (EvalError "Failed to convert object to expression")
      Just e -> ask >>= flip eval e

funcApply :: (MonadIO m, MonadEval n) => m (Object n)
funcApply =
  func \case
    [] -> throw (EvalError "Arity Mismatch")
    [_] -> throw (EvalError "Arity Mismatch")
    f : xs -> do
      args <- (init xs ++) <$> pairToList (last xs)
      apply f args
  where
    pairToList o = case o of
      Empty -> pure []
      Cons car cdr -> do
        o1 <- deref car
        o2 <- deref cdr
        os <- pairToList o2
        pure (o1 : os)
      _ -> pure [o]

funcIsNull :: (MonadIO m, MonadEval n) => m (Object n)
funcIsNull =
  mkFunc1 \case
    Empty -> pure true
    _ -> pure false

funcIsBool :: (MonadIO m, MonadEval n) => m (Object n)
funcIsBool =
  mkFunc1 \case
    Bool _ -> pure true
    _ -> pure false

funcIsNum :: (MonadIO m, MonadEval n) => m (Object n)
funcIsNum =
  mkFunc1 \case
    Num _ -> pure true
    _ -> pure false

funcIsStr :: (MonadIO m, MonadEval n) => m (Object n)
funcIsStr =
  mkFunc1 \case
    Str _ -> pure true
    _ -> pure false

funcIsSym :: (MonadIO m, MonadEval n) => m (Object n)
funcIsSym =
  mkFunc1 \case
    Sym _ -> pure true
    _ -> pure false

funcIsPair :: (MonadIO m, MonadEval n) => m (Object n)
funcIsPair =
  mkFunc1 \case
    Cons _ _ -> pure true
    _ -> pure false

funcIsProc :: (MonadIO m, MonadEval n) => m (Object n)
funcIsProc =
  mkFunc1 \case
    Func _ -> pure true
    _ -> pure false

funcAdd :: (MonadIO m, MonadEval n) => m (Object n)
funcAdd = mkNumFoldFunc (+) 0

funcMul :: (MonadIO m, MonadEval n) => m (Object n)
funcMul = mkNumFoldFunc (*) 1

funcSub :: (MonadIO m, MonadEval n) => m (Object n)
funcSub =
  func $
    traverse expectNum >=> \case
      [] -> throw (EvalError "expect at least one number")
      n : ns -> num $! n - sum ns

funcDiv :: (MonadIO m, MonadEval n) => m (Object n)
funcDiv =
  func $
    traverse expectNum >=> \case
      [] -> throw (EvalError "expect at least one number")
      n : ns -> do
        when (0 `elem` ns) do
          throw (EvalError "divided by zero")
        num $! n `div` product ns

funcMod :: (MonadIO m, MonadEval n) => m (Object n)
funcMod =
  mkFunc2 \x y -> do
    n <- expectNum x
    m <- expectNum y
    when (m == 0) do
      throw (EvalError "divided by zero")
    num $! n `mod` m

funcNumEq :: (MonadIO m, MonadEval n) => m (Object n)
funcNumEq = mkNumBinPred (==)

funcGt :: (MonadIO m, MonadEval n) => m (Object n)
funcGt = mkNumBinPred (>)

funcGe :: (MonadIO m, MonadEval n) => m (Object n)
funcGe = mkNumBinPred (>=)

funcLt :: (MonadIO m, MonadEval n) => m (Object n)
funcLt = mkNumBinPred (<)

funcLe :: (MonadIO m, MonadEval n) => m (Object n)
funcLe = mkNumBinPred (<=)

funcEq, funcEqv, funcEqual :: (MonadIO m, MonadEval n) => m (Object n)
funcEq = mkFunc2 eq
funcEqv = mkFunc2 eqv
funcEqual = mkFunc2 equal

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

funcStrAppend :: (MonadIO m, MonadEval n) => m (Object n)
funcStrAppend =
  func $
    traverse expectStr >=> \ts -> str $ T.concat ts

funcStrNum :: (MonadIO m, MonadEval n) => m (Object n)
funcStrNum =
  mkFunc1 $
    expectStr >=> \s -> case readSNum s of
      Just (SNum n) -> num n
      _ -> throw (EvalError "Failed to convert string to number")

funcNumStr :: (MonadIO m, MonadEval n) => m (Object n)
funcNumStr = mkFunc1 $ expectNum >=> str . T.pack . show

funcStrSym :: (MonadIO m, MonadEval n) => m (Object n)
funcStrSym = mkFunc1 $ expectStr >=> sym

funcSymStr :: (MonadIO m, MonadEval n) => m (Object n)
funcSymStr = mkFunc1 $ expectSym >=> str

funcGensym :: (MonadIO m, MonadEval n) => m (Object n)
funcGensym = mkFunc0 gensym

funcCons :: (MonadIO m, MonadEval n) => m (Object n)
funcCons = mkFunc2 cons

funcCar :: (MonadIO m, MonadEval n) => m (Object n)
funcCar = mkFunc1 $ expectCons >=> deref . fst

funcCdr :: (MonadIO m, MonadEval n) => m (Object n)
funcCdr = mkFunc1 $ expectCons >=> deref . snd

funcSetCar :: (MonadIO m, MonadEval n) => m (Object n)
funcSetCar =
  mkFunc2 \x y -> do
    (car, _) <- expectCons x
    undef <$ (car .= y)

funcSetCdr :: (MonadIO m, MonadEval n) => m (Object n)
funcSetCdr =
  mkFunc2 \x y -> do
    (_, cdr) <- expectCons x
    undef <$ (cdr .= y)

funcCallCC :: (MonadIO m, MonadEval n) => m (Object n)
funcCallCC =
  mkFunc1 \o ->
    callCC \k -> do
      c <- cont k
      apply o [c]

funcOpenInputFile :: (MonadIO m, MonadEval n) => m (Object n)
funcOpenInputFile = mkFileOpenFunc ReadMode

funcOpenOutputFile :: (MonadIO m, MonadEval n) => m (Object n)
funcOpenOutputFile = mkFileOpenFunc WriteMode

funcCloseInputPort :: (MonadIO m, MonadEval n) => m (Object n)
funcCloseInputPort =
  mkFunc1 \o -> do
    h <- expectPort o
    undef <$ liftIO (hClose h)

funcCloseOutputPort :: (MonadIO m, MonadEval n) => m (Object n)
funcCloseOutputPort = funcCloseInputPort

funcRead :: (MonadIO m, MonadEval n) => m (Object n)
funcRead =
  func \os -> do
    h <- case os of
      [] -> pure stdin
      [o] -> expectPort o
      _ -> throw (EvalError "Arity Mismatch")
    txt <- liftIO (T.hGetContents h)
    case readSExprList "" txt of
      Left e -> throw (ReadError e)
      Right es -> foldrM (\e o -> fromSExpr e >>= flip cons o) empty es

funcDisplay :: (MonadIO m, MonadEval n) => m (Object n)
funcDisplay =
  func \os -> do
    (o, h) <- case os of
      [o] -> pure (o, stdout)
      [o1, o2] -> (o1,) <$> expectPort o2
      _ -> throw (EvalError "ArityMismatch")
    undef <$ (display o >>= liftIO . T.hPutStr h)

funcWrite :: (MonadIO m, MonadEval n) => m (Object n)
funcWrite =
  func \os -> do
    (o, h) <- case os of
      [o] -> pure (o, stdout)
      [o1, o2] -> (o1,) <$> expectPort o2
      _ -> throw (EvalError "ArityMismatch")
    undef <$ (write o >>= liftIO . T.hPutStr h)

funcExit :: (MonadIO m, MonadEval n) => m (Object n)
funcExit = mkFunc0 (throw ExitSuccess)

funcError :: (MonadIO m, MonadEval n) => m (Object n)
funcError = func $ traverse display >=> throw . UserError . T.concat

-- smart constructors

mkFunc0 :: (MonadIO m, MonadEval n) => n (Object n) -> m (Object n)
mkFunc0 x =
  func \case
    [] -> x
    _ -> throw (EvalError "Arity Mismatch")

mkFunc1 :: (MonadIO m, MonadEval n) => (Object n -> n (Object n)) -> m (Object n)
mkFunc1 f =
  func \case
    [o] -> f o
    _ -> throw (EvalError "Arity Mismatch")

mkFunc2 :: (MonadIO m, MonadEval n) => (Object n -> Object n -> n (Object n)) -> m (Object n)
mkFunc2 f =
  func \case
    [o1, o2] -> f o1 o2
    _ -> throw (EvalError "Arity Mismatch")

mkNumFoldFunc :: (MonadIO m, MonadEval n) => (Integer -> Integer -> Integer) -> Integer -> m (Object n)
mkNumFoldFunc f z =
  func \os -> do
    ns <- traverse expectNum os
    num $! foldl' f z ns

mkNumBinPred :: (MonadIO m, MonadEval n) => (Integer -> Integer -> Bool) -> m (Object n)
mkNumBinPred p =
  mkFunc2 \o1 o2 -> do
    n1 <- expectNum o1
    n2 <- expectNum o2
    pure if p n1 n2 then true else false

mkFileOpenFunc :: (MonadIO m, MonadEval n) => IOMode -> m (Object n)
mkFileOpenFunc mode =
  mkFunc1 \o -> do
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
    Func f -> f xs
    Cont k ->
      case xs of
        [arg] -> k arg
        _ -> throw (EvalError "Arity mismatch")
    _ -> throw (EvalError "Could not apply")

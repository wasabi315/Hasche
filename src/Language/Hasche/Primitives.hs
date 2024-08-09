module Language.Hasche.Primitives (preparePrimitives) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Cont (callCC)
import Control.Monad.IO.Class
import Data.Foldable
import Data.Functor
import Data.Functor.Compose
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Traversable
import Language.Hasche.Error
import Language.Hasche.Eval
import Language.Hasche.Object
import Language.Hasche.Pattern
import Language.Hasche.Pretty
import Language.Hasche.Reader
import System.Exit
import System.IO
import Prelude hiding (pred)

preparePrimitives :: (MonadIO m, MonadThrow m) => EvalT r m ()
preparePrimitives = do
  bind "define" =<< synDefine
  bind "define-macro" =<< synDefMacro
  bind "quote" =<< synQuote
  bind "quasiquote" =<< synQuasiquote
  bind "unquote" =<< synUnquote
  bind "unquote-splicing" =<< synUnquoteSplicing
  bind "if" =<< synIf
  bind "set!" =<< synSet
  bind "lambda" =<< synLambda

  bind "match" =<< synMatch

  bind "eval" =<< funcEval
  bind "apply" =<< funcApply

  bind "null?" =<< funcIsNull
  bind "bool?" =<< funcIsBool
  bind "number?" =<< funcIsNum
  bind "string?" =<< funcIsStr
  bind "symbol?" =<< funcIsSym
  bind "pair?" =<< funcIsPair
  bind "procedure?" =<< funcIsProc

  bind "+" =<< funcAdd
  bind "-" =<< funcSub
  bind "*" =<< funcMul
  bind "/" =<< funcDiv
  bind "modulo" =<< funcMod
  bind "=" =<< funcNumEq
  bind "<" =<< funcLt
  bind "<=" =<< funcLe
  bind ">" =<< funcGt
  bind ">=" =<< funcGe

  bind "eq?" =<< funcEq
  bind "eqv?" =<< funcEqv
  bind "equal?" =<< funcEqual

  bind "string-append" =<< funcStrAppend
  bind "string->number" =<< funcStrNum
  bind "number->string" =<< funcNumStr
  bind "string->symbol" =<< funcStrSym
  bind "symbol->string" =<< funcSymStr

  bind "gensym" =<< funcGensym

  bind "cons" =<< funcCons
  bind "car" =<< funcCar
  bind "cdr" =<< funcCdr
  bind "set-car!" =<< funcSetCar
  bind "set-cdr!" =<< funcSetCdr

  bind "call/cc" =<< funcCallCC
  bind "dynamic-wind" =<< funcDynamicWind

  bind "open-input-file" =<< funcOpenInputFile
  bind "open-output-file" =<< funcOpenOutputFile
  bind "close-input-port" =<< funcCloseInputPort
  bind "close-output-port" =<< funcCloseOutputPort
  bind "read" =<< funcRead
  bind "display" =<< funcDisplay
  bind "write" =<< funcWrite

  bind "error" =<< funcError

  bind "exit" =<< funcExit

funcEval :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcEval = mkFunc1 $ withToplevelEnv . eval

funcApply :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcApply = func \case
  [] -> throw EArityMismatch
  [_] -> throw EArityMismatch
  f : args -> do
    args' <- (init args ++) <$> pairToList (last args)
    apply f args'
  where
    pairToList obj =
      listify obj <&> \case
        NonList obj' -> [obj']
        List obj' -> obj'
        DList objs obj' -> NE.toList objs ++ [obj']

funcIsNull :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcIsNull = mkFunc1 \case
  Empty -> pure true
  _ -> pure false

funcIsBool :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcIsBool = mkFunc1 \case
  Bool _ -> pure true
  _ -> pure false

funcIsNum :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcIsNum = mkFunc1 \case
  Num _ -> pure true
  _ -> pure false

funcIsStr :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcIsStr = mkFunc1 \case
  Str _ -> pure true
  _ -> pure false

funcIsSym :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcIsSym = mkFunc1 \case
  Sym _ -> pure true
  _ -> pure false

funcIsPair :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcIsPair = mkFunc1 \case
  Cons _ _ -> pure true
  _ -> pure false

funcIsProc :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcIsProc = mkFunc1 \case
  Func _ -> pure true
  _ -> pure false

funcAdd :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcAdd = mkNumFoldFunc (+) 0

funcMul :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcMul = mkNumFoldFunc (*) 1

funcSub :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcSub = func do
  traverse expectNum >=> \case
    [] -> throw EArityMismatch
    n : ns -> num $! n - foldl' (+) 0 ns

funcDiv :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcDiv = func do
  traverse expectNum >=> \case
    [] -> throw EArityMismatch
    n : ns -> do
      let d = foldl' (*) 1 ns
      when (d == 0) $ throw EDividedByZero
      num $! n `div` d

funcMod :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcMod = mkFunc2 \x y -> do
  n <- expectNum x
  m <- expectNum y
  when (m == 0) $ throw EDividedByZero
  num $! n `mod` m

funcNumEq :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcNumEq = mkNumBinPred (==)

funcGt :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcGt = mkNumBinPred (>)

funcGe :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcGe = mkNumBinPred (>=)

funcLt :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcLt = mkNumBinPred (<)

funcLe :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcLe = mkNumBinPred (<=)

funcEq, funcEqv, funcEqual :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcEq = mkFunc2 eq
funcEqv = mkFunc2 eqv
funcEqual = mkFunc2 equal

eq :: (MonadIO m) => Object (EvalT r m) -> Object (EvalT r m) -> EvalT r m (Object (EvalT r m))
eq x y = pure if objID x == objID y then true else false

eqv :: (MonadIO m) => Object (EvalT r m) -> Object (EvalT r m) -> EvalT r m (Object (EvalT r m))
eqv (Num n) (Num m) = pure if n == m then true else false
eqv x y = eq x y

equal :: forall r m. (MonadIO m, MonadThrow m) => Object (EvalT r m) -> Object (EvalT r m) -> EvalT r m (Object (EvalT r m))
equal (Cons ref1 ref2) (Cons ref3 ref4) = do
  obj1 <- deref ref1
  obj3 <- deref ref3
  equal obj1 obj3 >>= \case
    Bool False -> pure false
    _ -> do
      obj2 <- deref ref2
      obj4 <- deref ref4
      equal obj2 obj4
equal x y = eqv x y

funcStrAppend :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcStrAppend = func $ traverse expectStr >=> str . T.concat

funcStrNum :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcStrNum = mkFunc1 $ expectStr >=> readNum

funcNumStr :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcNumStr = mkFunc1 $ expectNum >=> str . T.pack . show

funcStrSym :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcStrSym = mkFunc1 $ expectStr >=> sym

funcSymStr :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcSymStr = mkFunc1 $ expectSym >=> str

funcGensym :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcGensym = mkFunc0 freshSym

funcCons :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcCons = mkFunc2 cons

funcCar :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcCar = mkFunc1 $ expectCons >=> deref . fst

funcCdr :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcCdr = mkFunc1 $ expectCons >=> deref . snd

funcSetCar :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcSetCar = mkFunc2 \x y -> do
  (car, _) <- expectCons x
  undef <$ setRef car y

funcSetCdr :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcSetCdr = mkFunc2 \x y -> do
  (_, cdr) <- expectCons x
  undef <$ setRef cdr y

funcCallCC :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcCallCC = mkFunc1 \arg -> callCC \k -> do
  curr <- getCurrDWAction
  k' <- cont curr k
  apply arg (pure k')

funcDynamicWind :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcDynamicWind = mkFunc3 \arg arg' arg'' -> do
  let pre = () <$ apply arg []
      post = () <$ apply arg'' []
  withDWAction pre post (apply arg' [])

funcOpenInputFile :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcOpenInputFile = mkFileOpenFunc ReadMode

funcOpenOutputFile :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcOpenOutputFile = mkFileOpenFunc WriteMode

funcCloseInputPort :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcCloseInputPort = mkFunc1 \arg -> do
  h <- expectPort arg
  undef <$ liftIO (hClose h)

funcCloseOutputPort :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcCloseOutputPort = funcCloseInputPort

funcRead :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcRead = func \args -> do
  h <- case args of
    [] -> pure stdin
    [arg] -> expectPort arg
    _ -> throw EArityMismatch
  txt <- liftIO (T.hGetContents h)
  list =<< readObjectList "" txt

funcDisplay :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcDisplay = func \args -> do
  (obj, h) <- case args of
    [obj] -> pure (obj, stdout)
    [obj, obj'] -> (obj,) <$> expectPort obj'
    _ -> throw EArityMismatch
  undef <$ (display obj >>= liftIO . T.hPutStr h)

funcWrite :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcWrite = func \args -> do
  (obj, h) <- case args of
    [o] -> pure (o, stdout)
    [obj, obj2] -> (obj,) <$> expectPort obj2
    _ -> throw EArityMismatch
  undef <$ (write obj >>= liftIO . T.hPutStr h)

funcExit :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcExit = mkFunc0 $ throw ExitSuccess

funcError :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
funcError = func $ traverse display >=> throw . EUser . T.concat

-- smart constructors

mkFunc0 ::
  (MonadIO m, MonadThrow m) =>
  EvalT r m (Object (EvalT r m)) ->
  EvalT r m (Object (EvalT r m))
mkFunc0 x = func \case
  [] -> x
  _ -> throw EArityMismatch

mkFunc1 ::
  (MonadIO m, MonadThrow m) =>
  (Object (EvalT r m) -> EvalT r m (Object (EvalT r m))) ->
  EvalT r m (Object (EvalT r m))
mkFunc1 f = func \case
  [arg] -> f arg
  _ -> throw EArityMismatch

mkFunc2 ::
  (MonadIO m, MonadThrow m) =>
  (Object (EvalT r m) -> Object (EvalT r m) -> EvalT r m (Object (EvalT r m))) ->
  EvalT r m (Object (EvalT r m))
mkFunc2 f = func \case
  [arg, arg'] -> f arg arg'
  _ -> throw EArityMismatch

mkFunc3 ::
  (MonadIO m, MonadThrow m) =>
  (Object (EvalT r m) -> Object (EvalT r m) -> Object (EvalT r m) -> EvalT r m (Object (EvalT r m))) ->
  EvalT r m (Object (EvalT r m))
mkFunc3 f = func \case
  [arg, arg', arg''] -> f arg arg' arg''
  _ -> throw EArityMismatch

mkNumFoldFunc :: (MonadIO m, MonadThrow m) => (Integer -> Integer -> Integer) -> Integer -> EvalT r m (Object (EvalT r m))
mkNumFoldFunc f z = func \args -> do
  ns <- traverse expectNum args
  num $! foldl' f z ns

mkNumBinPred :: (MonadIO m, MonadThrow m) => (Integer -> Integer -> Bool) -> EvalT r m (Object (EvalT r m))
mkNumBinPred pred = mkFunc2 \arg arg' -> do
  n <- expectNum arg
  n' <- expectNum arg'
  pure if pred n n' then true else false

mkFileOpenFunc :: (MonadIO m, MonadThrow m) => IOMode -> EvalT r m (Object (EvalT r m))
mkFileOpenFunc mode = mkFunc1 \obj -> do
  path <- expectStr obj
  h <- liftIO $ openFile (T.unpack path) mode `catchIO` (throw . EIO)
  port h

synQuote :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
synQuote = syn \case
  [obj] -> pure obj
  _ -> throw ESyntax

synQuasiquote :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
synQuasiquote = syn \case
  [obj] -> qq obj
  _ -> throw ESyntax
  where
    qq =
      listify >=> \case
        NonList obj -> pure obj
        List [Sym "unquote", obj] -> eval obj
        List [Sym "unquote-splicing", _] -> throw ESyntax
        List objs -> traverse qqSplicing objs >>= list . concat
        DList objs obj -> do
          objs' <- traverse qqSplicing objs
          obj' <- qq obj
          dlist (NE.fromList $ concat objs') obj'

    qqSplicing =
      listify >=> \case
        NonList obj -> pure [obj]
        List [Sym "unquote", obj] -> pure <$> eval obj
        List [Sym "unquote-splicing", obj] -> eval obj >>= expectList
        List objs -> traverse qqSplicing objs >>= fmap pure . list . concat
        DList objs obj -> do
          objs' <- traverse qqSplicing objs
          obj' <- qq obj
          pure <$> dlist (NE.fromList $ concat objs') obj'

synUnquote :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
synUnquote = syn \_ -> throw ESyntax

synUnquoteSplicing :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
synUnquoteSplicing = syn \_ -> throw ESyntax

synIf :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
synIf = syn \case
  [cond, then'] -> do
    eval cond >>= \case
      Bool False -> pure undef
      _ -> eval then'
  [cond, then', else'] -> do
    eval cond >>= \case
      Bool False -> eval else'
      _ -> eval then'
  _ -> throw ESyntax

synSet :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
synSet = syn \case
  [Sym var, expr] -> do
    obj <- eval expr
    undef <$ set var obj
  _ -> throw ESyntax

synDefine :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
synDefine = syn \case
  [Sym name, expr] -> do
    obj <- eval expr
    undef <$ bind name obj
  Cons name params : body -> do
    name' <- expectSym =<< deref name
    cls <- deref params >>= flip mkClosure body
    undef <$ bind name' cls
  _ -> throw ESyntax

synDefMacro :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
synDefMacro = syn \es -> do
  (name, s) <- case es of
    [Sym name, expr] -> do
      obj <- eval expr
      _ <- expectSyn obj
      pure (name, obj)
    Cons name params : body -> do
      name' <- expectSym =<< deref name
      cls <- deref params >>= flip mkClosure body >>= expectFunc
      (name',) <$> syn (cls >=> eval)
    _ -> throw ESyntax
  undef <$ bind name s

synLambda :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
synLambda = syn \case
  params : body -> mkClosure params body
  _ -> throw ESyntax

mkClosure ::
  (MonadIO m, MonadThrow m) =>
  Object (EvalT r m) ->
  [Object (EvalT r m)] ->
  EvalT r m (Object (EvalT r m))
mkClosure = \params body -> do
  binder <- listify params >>= mkBinder
  checkBodySyntax body
  closure \args -> binder args *> evalMany body
  where
    bind' param arg = expectSym param >>= flip bind arg

    mkBinder (NonList param) = pure $ list >=> bind' param
    mkBinder (List params) = pure \args -> do
      when (length params /= length args) $ throw EArityMismatch
      zipWithM_ bind' params args
    mkBinder (DList params param) = pure \args -> do
      let paramsLen = length params
      when (paramsLen > length args) $ throw EArityMismatch
      let (args1, args2) = splitAt paramsLen args
      zipWithM_ bind' (NE.toList params) args1
      list args2 >>= bind' param

    -- body : (define ...)* expression*
    checkBodySyntax =
      void . flip foldlM True \isDefinePart -> \case
        (Cons ref _) ->
          deref ref >>= \case
            Sym "define"
              | not isDefinePart -> throw ESyntax
              | otherwise -> pure True
            _ -> pure False
        _ -> pure False

synMatch :: (MonadIO m, MonadThrow m) => EvalT r m (Object (EvalT r m))
synMatch = syn \case
  [] -> throw ESyntax
  expr : arms ->
    parseArms arms >>= match expr . fromMaybe (throw ESyntax)
  where
    parseArms =
      getCompose . traverse \arm -> Compose do
        listify arm <&> \case
          List [pat, expr] -> Just (pat, expr)
          _ -> Nothing

-- utils

expectNum :: (MonadThrow m) => Object (EvalT r m) -> EvalT r m Integer
expectNum = \case
  Num n -> pure n
  _ -> throw EType

expectStr :: (MonadThrow m) => Object (EvalT r m) -> EvalT r m T.Text
expectStr = \case
  Str s -> pure s
  _ -> throw EType

expectSym :: (MonadThrow m) => Object (EvalT r m) -> EvalT r m T.Text
expectSym = \case
  Sym s -> pure s
  _ -> throw EType

expectPort :: (MonadThrow m) => Object (EvalT r m) -> EvalT r m Handle
expectPort = \case
  Port h -> pure h
  _ -> throw EType

expectCons :: (MonadThrow m) => Object (EvalT r m) -> EvalT r m (ObjRef (EvalT r m), ObjRef (EvalT r m))
expectCons = \case
  Cons car cdr -> pure (car, cdr)
  _ -> throw EType

expectList :: (MonadThrow m, MonadIO m) => Object (EvalT r m) -> EvalT r m [Object (EvalT r m)]
expectList =
  listify >=> \case
    List xs -> pure xs
    _ -> throw EType

expectFunc :: (MonadThrow m) => Object (EvalT r m) -> EvalT r m ([Object (EvalT r m)] -> EvalT r m (Object (EvalT r m)))
expectFunc = \case
  Func f -> pure f
  _ -> throw EType

expectSyn :: (MonadThrow m) => Object (EvalT r m) -> EvalT r m ([Object (EvalT r m)] -> EvalT r m (Object (EvalT r m)))
expectSyn = \case
  Syn f -> pure f
  _ -> throw EType

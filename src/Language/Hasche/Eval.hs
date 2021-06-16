{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Language.Hasche.Eval
  ( EvalM,
    runEvalM,
    evaluate,
    Object,
    pretty,
  )
where

import Control.Exception.Safe
import Control.Monad.Cont
import Control.Monad.Reader
import Data.Text (Text)
import Language.Hasche.Eval.Error
import Language.Hasche.Eval.Eval
import Language.Hasche.Eval.Format
import Language.Hasche.Eval.Object hiding (Object)
import Language.Hasche.Eval.Object qualified as Obj (Object)
import Language.Hasche.Syntax.SExpr

newtype EvalM r a = EvalM (ReaderT (Env (EvalM r)) (ContT r IO) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCont,
      MonadReader (Env (EvalM r))
    )

runEvalM :: EvalM r a -> Env (EvalM r) -> (a -> IO r) -> IO r
runEvalM (EvalM m) = runContT . runReaderT m

evaluate :: Env (EvalM (Either Error Object)) -> [SExpr] -> IO (Either Error Object)
evaluate env prog =
  catch
    (runEvalM (evalMany env prog) env (pure . Right . Object))
    (pure . Left)

data Object = forall m. Object (Obj.Object m)

pretty :: Object -> IO Text
pretty (Object obj) = write obj

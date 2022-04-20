{-# LANGUAGE BlockArguments #-}

module Language.Hasche.Builtins (builtinEnv) where

import Control.Monad.IO.Class
import Language.Hasche.Builtins.Primitives
import Language.Hasche.Builtins.SpecialForms
import Language.Hasche.Eval.Env qualified as Env
import Language.Hasche.Eval.Eval
import Language.Hasche.Eval.Object

builtinEnv :: (MonadIO m, MonadEval n) => m (Env n)
builtinEnv = do
  e <- Env.empty

  Env.bind e "define" =<< synDefine
  Env.bind e "define-macro" =<< synDefMacro
  Env.bind e "quote" =<< synQuote
  Env.bind e "quasiquote" =<< synQuasiquote
  Env.bind e "unquote" =<< synUnquote
  Env.bind e "unquote-splicing" =<< synUnquoteSplicing
  Env.bind e "if" =<< synIf
  Env.bind e "set!" =<< synSet
  Env.bind e "lambda" =<< synLambda

  Env.bind e "match" =<< synMatch

  Env.bind e "eval" =<< funcEval
  Env.bind e "apply" =<< funcApply

  Env.bind e "null?" =<< funcIsNull
  Env.bind e "bool?" =<< funcIsBool
  Env.bind e "number?" =<< funcIsNum
  Env.bind e "string?" =<< funcIsStr
  Env.bind e "symbol?" =<< funcIsSym
  Env.bind e "pair?" =<< funcIsPair
  Env.bind e "procedure?" =<< funcIsProc

  Env.bind e "+" =<< funcAdd
  Env.bind e "-" =<< funcSub
  Env.bind e "*" =<< funcMul
  Env.bind e "/" =<< funcDiv
  Env.bind e "modulo" =<< funcMod
  Env.bind e "=" =<< funcNumEq
  Env.bind e "<" =<< funcLt
  Env.bind e "<=" =<< funcLe
  Env.bind e ">" =<< funcGt
  Env.bind e ">=" =<< funcGe

  Env.bind e "eq?" =<< funcEq
  Env.bind e "eqv?" =<< funcEqv
  Env.bind e "equal?" =<< funcEqual

  Env.bind e "string-append" =<< funcStrAppend
  Env.bind e "string->number" =<< funcStrNum
  Env.bind e "number->string" =<< funcNumStr
  Env.bind e "string->symbol" =<< funcStrSym
  Env.bind e "symbol->string" =<< funcSymStr

  Env.bind e "gensym" =<< funcGensym

  Env.bind e "cons" =<< funcCons
  Env.bind e "car" =<< funcCar
  Env.bind e "cdr" =<< funcCdr
  Env.bind e "set-car!" =<< funcSetCar
  Env.bind e "set-cdr!" =<< funcSetCdr

  Env.bind e "call/cc" =<< funcCallCC

  Env.bind e "open-input-file" =<< funcOpenInputFile
  Env.bind e "open-output-file" =<< funcOpenOutputFile
  Env.bind e "close-input-port" =<< funcCloseInputPort
  Env.bind e "close-output-port" =<< funcCloseOutputPort
  Env.bind e "read" =<< funcRead
  Env.bind e "display" =<< funcDisplay
  Env.bind e "write" =<< funcWrite

  Env.bind e "error" =<< funcError

  Env.bind e "exit" =<< funcExit

  pure e

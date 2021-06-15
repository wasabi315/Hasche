{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Hasche.Builtins (builtinEnv) where

import Control.Monad.IO.Class
import Language.Hasche.Builtins.Primitives
import Language.Hasche.Builtins.SpecialForms
import Language.Hasche.Eval
import Language.Hasche.Object

builtinEnv :: (MonadIO m, MonadEval n) => m (Env n)
builtinEnv = do
  e <- rootEnv

  bind e "define" =<< synDefine
  bind e "define-macro" =<< synDefMacro
  bind e "quote" =<< synQuote
  bind e "quasiquote" =<< synQuasiquote
  bind e "unquote" =<< synUnquote
  bind e "unquote-splicing" =<< synUnquoteSplicing
  bind e "if" =<< synIf
  bind e "set!" =<< synSet
  bind e "lambda" =<< synLambda

  bind e "match" =<< synMatch

  bind e "eval" =<< funcEval
  bind e "apply" =<< funcApply

  bind e "null?" =<< funcIsNull
  bind e "bool?" =<< funcIsBool
  bind e "number?" =<< funcIsNum
  bind e "string?" =<< funcIsStr
  bind e "symbol?" =<< funcIsSym
  bind e "pair?" =<< funcIsPair
  bind e "procedure?" =<< funcIsProc

  bind e "+" =<< funcAdd
  bind e "-" =<< funcSub
  bind e "*" =<< funcMul
  bind e "/" =<< funcDiv
  bind e "modulo" =<< funcMod
  bind e "=" =<< funcNumEq
  bind e "<" =<< funcLt
  bind e "<=" =<< funcLe
  bind e ">" =<< funcGt
  bind e ">=" =<< funcGe

  bind e "eq?" =<< funcEq
  bind e "eqv?" =<< funcEqv
  bind e "equal?" =<< funcEqual

  bind e "string-append" =<< funcStrAppend
  bind e "string->number" =<< funcStrNum
  bind e "number->string" =<< funcNumStr
  bind e "string->symbol" =<< funcStrSym
  bind e "symbol->string" =<< funcSymStr

  bind e "gensym" =<< funcGensym

  bind e "cons" =<< funcCons
  bind e "car" =<< funcCar
  bind e "cdr" =<< funcCdr
  bind e "set-car!" =<< funcSetCar
  bind e "set-cdr!" =<< funcSetCdr

  bind e "call/cc" =<< funcCallCC

  bind e "open-input-file" =<< funcOpenInputFile
  bind e "open-output-file" =<< funcOpenOutputFile
  bind e "close-input-port" =<< funcCloseInputPort
  bind e "close-output-port" =<< funcCloseOutputPort
  bind e "read" =<< funcRead
  bind e "display" =<< funcDisplay
  bind e "write" =<< funcWrite

  bind e "error" =<< funcError

  bind e "exit" =<< funcExit

  pure e

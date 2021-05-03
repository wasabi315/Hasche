{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasche.Builtins (builtinEnv) where

import Control.Monad.IO.Class
import Hasche.Builtins.Primitives
import Hasche.Builtins.SpecialForms
import Hasche.Eval
import Hasche.Object

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

  bind e "eval" =<< primEval
  bind e "apply" =<< primApply

  bind e "null?" =<< primIsNull
  bind e "bool?" =<< primIsBool
  bind e "number?" =<< primIsNum
  bind e "string?" =<< primIsStr
  bind e "symbol?" =<< primIsSym
  bind e "pair?" =<< primIsPair
  bind e "procedure?" =<< primIsProc

  bind e "+" =<< primAdd
  bind e "-" =<< primSub
  bind e "*" =<< primMul
  bind e "/" =<< primDiv
  bind e "=" =<< primNumEq
  bind e "<" =<< primLt
  bind e "<=" =<< primLe
  bind e ">" =<< primGt
  bind e ">=" =<< primGe

  bind e "eq?" =<< primEq
  bind e "eqv?" =<< primEqv
  bind e "equal?" =<< primEqual

  bind e "string-append" =<< primStrAppend
  bind e "string->number" =<< primStrNum
  bind e "number->string" =<< primNumStr
  bind e "string->symbol" =<< primStrSym
  bind e "symbol->string" =<< primSymStr

  bind e "gensym" =<< primGensym

  bind e "cons" =<< primCons
  bind e "car" =<< primCar
  bind e "cdr" =<< primCdr
  bind e "set-car!" =<< primSetCar
  bind e "set-cdr!" =<< primSetCdr

  bind e "call/cc" =<< primCallCC

  bind e "open-input-file" =<< primOpenInputFile
  bind e "open-output-file" =<< primOpenOutputFile
  bind e "close-input-port" =<< primCloseInputPort
  bind e "close-output-port" =<< primCloseOutputPort
  bind e "read" =<< primRead
  bind e "display" =<< primDisplay
  bind e "write" =<< primWrite

  bind e "exit" =<< primExit

  pure e

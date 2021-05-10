{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module Hasche.Format
  ( write,
    display,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.HashTable.IO (BasicHashTable)
import Data.HashTable.IO qualified as HT
import Data.IORef
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TB
import Hasche.Box
import Hasche.Object qualified as Obj

-- Stringify Object

write :: MonadIO m => Obj.Object n -> m Text
write = format writeOption

display :: MonadIO m => Obj.Object n -> m Text
display = format displayOption

data FormatOption = FormatOption
  { undef :: Builder,
    empty :: Builder,
    bool :: Bool -> Builder,
    num :: Integer -> Builder,
    str :: Text -> Builder,
    sym :: Text -> Builder,
    port :: Builder,
    syn :: Builder,
    func :: Builder,
    cont :: Builder,
    ref :: Label -> Builder,
    cons :: (Decycled -> Builder) -> Maybe Label -> Decycled -> Decycled -> Builder
  }

format :: MonadIO m => FormatOption -> Obj.Object n -> m Text
format FormatOption {..} obj =
  TL.toStrict . TB.toLazyText . format' <$!> liftIO (decycle obj)
  where
    format' = \case
      DUndef -> undef
      DEmpty -> empty
      DBool b -> bool b
      DNum n -> num n
      DStr s -> str s
      DSym s -> sym s
      DPort -> port
      DSyn -> syn
      DFunc -> func
      DCont -> cont
      DRef l -> ref l
      DCons ml d1 d2 -> cons format' ml d1 d2

writeOption :: FormatOption
writeOption =
  FormatOption
    { undef = "#<undef>",
      empty = "()",
      bool = \b -> if b then "#t" else "#f",
      num = TB.decimal,
      str = TB.fromString . show,
      sym = TB.fromText,
      port = "#<port>",
      syn = "#<syntax>",
      func = "#<procedure>",
      cont = "#<continuation>",
      ref = \l -> "#" <> TB.decimal l <> "#",
      cons = \fmt ->
        let fmtCons Nothing (DSym "quote") (DCons Nothing d DEmpty) =
              "'" <> fmt d
            fmtCons Nothing (DSym "quasiquote") (DCons Nothing d DEmpty) =
              "`" <> fmt d
            fmtCons Nothing (DSym "unquote") (DCons Nothing d DEmpty) =
              "," <> fmt d
            fmtCons Nothing (DSym "unquote-splicing") (DCons Nothing d DEmpty) =
              ",@" <> fmt d
            fmtCons (Just l) car cdr =
              "#" <> TB.decimal l <> "=(" <> fmtList car cdr <> ")"
            fmtCons Nothing car cdr =
              "(" <> fmtList car cdr <> ")"

            fmtList d DEmpty = fmt d
            fmtList d1 (DCons Nothing d2 d3) = fmt d1 <> " " <> fmtList d2 d3
            fmtList d1 d2 = fmt d1 <> " . " <> fmt d2
         in fmtCons
    }

displayOption :: FormatOption
displayOption =
  writeOption
    { str = TB.fromText
    }

-- Decycling: replace circular reference with label

data Decycled
  = DUndef
  | DEmpty
  | DBool Bool
  | DNum Integer
  | DStr Text
  | DSym Text
  | DPort
  | DSyn
  | DFunc
  | DCont
  | DRef Label
  | DCons (Maybe Label) Decycled Decycled
  deriving (Show)

type Label = Int

data DecycleStatus
  = InProgress
  | CycleDetected Label
  | Done Decycled

decycle :: Obj.Object n -> IO Decycled
decycle obj = do
  table <- HT.new :: IO (BasicHashTable Loc DecycleStatus)
  nextLabel <- newIORef (0 :: Label)

  let loop Obj.Undef = pure DUndef
      loop Obj.Empty = pure DEmpty
      loop (Obj.Bool b) = pure (DBool b)
      loop (Obj.Num n) = pure (DNum n)
      loop (Obj.Str s) = pure (DStr s)
      loop (Obj.Sym s) = pure (DSym s)
      loop (Obj.Port _) = pure DPort
      loop (Obj.Syn _) = pure DSyn
      loop (Obj.Func _) = pure DFunc
      loop (Obj.Cont _) = pure DCont
      loop cons@(Obj.Cons car cdr) =
        HT.lookup table (loc cons) >>= \case
          Nothing -> do
            HT.insert table (loc cons) InProgress
            d1 <- Obj.deref car >>= loop
            d2 <- Obj.deref cdr >>= loop
            HT.mutate table (loc cons) \mres ->
              let d = case mres of
                    Just InProgress -> DCons Nothing d1 d2
                    Just (CycleDetected l) -> DCons (Just l) d1 d2
                    _ -> error "Unreachable"
               in (Just (Done d), d)
          Just InProgress -> do
            l <- readIORef nextLabel <* modifyIORef' nextLabel succ
            HT.insert table (loc cons) (CycleDetected l)
            pure (DRef l)
          Just (CycleDetected l) -> pure (DRef l)
          Just (Done d) -> pure d

  loop obj

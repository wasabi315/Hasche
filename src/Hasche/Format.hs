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
    prim :: Builder,
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
      DPrim -> prim
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
      prim = "#<primitive>",
      func = "#<procedure>",
      cont = "#<continuation>",
      ref = \l -> "#" <> TB.decimal l <> "#",
      cons = \fmt ml car cdr ->
        let loop d DEmpty = fmt d
            loop d1 (DCons Nothing d2 d3) = fmt d1 <> " " <> loop d2 d3
            loop d1 d2 = fmt d1 <> " . " <> fmt d2
         in case (ml, car, cdr) of
              (Just l, _, _) ->
                "#" <> TB.decimal l <> "=(" <> loop car cdr <> ")"
              (Nothing, DSym "quote", DCons Nothing d DEmpty) ->
                "'" <> fmt d
              (Nothing, DSym "quasiquote", DCons Nothing d DEmpty) ->
                "`" <> fmt d
              (Nothing, DSym "unquote", DCons Nothing d DEmpty) ->
                "," <> fmt d
              (Nothing, DSym "unquote-splicing", DCons Nothing d DEmpty) ->
                ",@" <> fmt d
              _ ->
                "(" <> loop car cdr <> ")"
    }

displayOption :: FormatOption
displayOption =
  writeOption
    { str = TB.fromText
    }

-- Cyclic list detection

type Label = Int

data Decycled
  = DUndef
  | DEmpty
  | DBool Bool
  | DNum Integer
  | DStr Text
  | DSym Text
  | DPort
  | DSyn
  | DPrim
  | DFunc
  | DCont
  | DRef Label
  | DCons (Maybe Label) Decycled Decycled
  deriving (Show)

data VisitResult
  = Seen
  | CycleDetected Label
  | Finished Decycled

decycle :: Obj.Object n -> IO Decycled
decycle obj = do
  table <- HT.new :: IO (BasicHashTable Loc VisitResult)
  nextLabel <- newIORef (0 :: Label)

  let loop Obj.Undef = pure DUndef
      loop Obj.Empty = pure DEmpty
      loop (Obj.Bool b) = pure (DBool b)
      loop (Obj.Num n) = pure (DNum n)
      loop (Obj.Str s) = pure (DStr s)
      loop (Obj.Sym s) = pure (DSym s)
      loop (Obj.Port _) = pure DPort
      loop (Obj.Syn _) = pure DSyn
      loop (Obj.Prim _) = pure DPrim
      loop (Obj.Func _ _) = pure DFunc
      loop (Obj.Cont _) = pure DCont
      loop cons@(Obj.Cons car cdr) = do
        HT.insert table (loc cons) Seen
        d1 <- Obj.deref car >>= loop'
        d2 <- Obj.deref cdr >>= loop'
        Just res <- HT.lookup table (loc cons)
        let d = case res of
              Seen -> DCons Nothing d1 d2
              CycleDetected l -> DCons (Just l) d1 d2
              Finished _ -> error "Unreachable"
        HT.insert table (loc cons) (Finished d)
        pure d

      loop' cons@(Obj.Cons _ _) = do
        mres <- HT.lookup table (loc cons)
        case mres of
          Nothing -> loop cons
          Just (Finished d) -> pure d
          Just (CycleDetected l) -> pure (DRef l)
          Just Seen -> do
            l <- readIORef nextLabel <* modifyIORef' nextLabel succ
            HT.insert table (loc cons) (CycleDetected l)
            pure (DRef l)
      loop' o = loop o

  loop obj

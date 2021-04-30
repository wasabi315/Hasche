{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Hasche.Format
  ( write,
    display,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TB
import Hasche.Object qualified as Obj

write :: MonadIO m => Obj.ObjRef n -> m Text
write = format writeOption

display :: MonadIO m => Obj.ObjRef n -> m Text
display = format displayOption

data FormatOption m = FormatOption
  { undef :: Builder,
    empty :: Builder,
    bool :: Bool -> Builder,
    num :: Integer -> Builder,
    str :: Text -> Builder,
    sym :: Text -> Builder,
    cons :: forall n. (Obj.ObjRef n -> m Builder) -> Obj.ObjRef n -> Obj.ObjRef n -> m Builder,
    syn :: Builder,
    prim :: Builder,
    func :: Builder,
    cont :: Builder
  }

format :: MonadIO m => FormatOption m -> Obj.ObjRef n -> m Text
format FormatOption {..} obj = TL.toStrict . TB.toLazyText <$!> format' obj
  where
    format' =
      Obj.deref >=> \case
        Obj.Undef -> pure undef
        Obj.Empty -> pure empty
        Obj.Bool b -> pure (bool b)
        Obj.Num n -> pure (num n)
        Obj.Str s -> pure (str s)
        Obj.Sym s -> pure (sym s)
        Obj.Cons r1 r2 -> formatCons r1 r2
        Obj.Syn _ -> pure syn
        Obj.Prim _ -> pure prim
        Obj.Func _ _ -> pure func
        Obj.Cont _ -> pure cont
    formatCons = cons format'

writeOption :: MonadIO m => FormatOption m
writeOption =
  FormatOption
    { undef = "#<undef>",
      empty = "()",
      bool = \b -> if b then "#t" else "#f",
      num = TB.decimal,
      str = TB.fromString . show,
      sym = TB.fromText,
      cons = \fmt car cdr -> do
        let loop r1 r2 = do
              t1 <- fmt r1
              Obj.deref r2 >>= \case
                Obj.Empty ->
                  pure t1
                Obj.Cons r3 r4 ->
                  ((t1 <> TB.singleton ' ') <>) <$!> loop r3 r4
                _ ->
                  ((t1 <> " . ") <>) <$!> fmt r2
        o1 <- Obj.deref car
        o2 <- Obj.deref cdr
        case (o1, o2) of
          (Obj.Sym "quote", Obj.Cons r1 _) ->
            (TB.singleton '\'' <>) <$!> fmt r1
          _ ->
            (\t -> TB.singleton '(' <> t <> TB.singleton ')') <$!> loop car cdr,
      syn = "#<syntax>",
      prim = "#<primitive>",
      func = "#<procedure>",
      cont = "#<continuation>"
    }

displayOption :: forall m. MonadIO m => FormatOption m
displayOption =
  (writeOption @m)
    { str = TB.fromText
    }

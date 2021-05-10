{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

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
import Hasche.Object

-- Stringify Object

write :: MonadIO m => Object n -> m Text
write = format writeOption

display :: MonadIO m => Object n -> m Text
display = format displayOption

data FormatOption = FormatOption
  { fmtUndef :: Builder,
    fmtEmpty :: Builder,
    fmtBool :: Bool -> Builder,
    fmtNum :: Integer -> Builder,
    fmtStr :: Text -> Builder,
    fmtSym :: Text -> Builder,
    fmtPort :: Builder,
    fmtSyn :: Builder,
    fmtFunc :: Builder,
    fmtCont :: Builder,
    fmtRef :: Label -> Builder,
    fmtCons :: (Decycled -> Builder) -> Maybe Label -> Decycled -> Decycled -> Builder
  }

format :: MonadIO m => FormatOption -> Object n -> m Text
format FormatOption {..} obj =
  TL.toStrict . TB.toLazyText . format' <$!> liftIO (decycle obj)
  where
    format' = \case
      DUndef -> fmtUndef
      DEmpty -> fmtEmpty
      DBool b -> fmtBool b
      DNum n -> fmtNum n
      DStr s -> fmtStr s
      DSym s -> fmtSym s
      DPort -> fmtPort
      DSyn -> fmtSyn
      DFunc -> fmtFunc
      DCont -> fmtCont
      DRef l -> fmtRef l
      DCons ml d1 d2 -> fmtCons format' ml d1 d2

writeOption :: FormatOption
writeOption =
  FormatOption
    { fmtUndef = "#<undef>",
      fmtEmpty = "()",
      fmtBool = \b -> if b then "#t" else "#f",
      fmtNum = TB.decimal,
      fmtStr = TB.fromString . show,
      fmtSym = TB.fromText,
      fmtPort = "#<port>",
      fmtSyn = "#<syntax>",
      fmtFunc = "#<procedure>",
      fmtCont = "#<continuation>",
      fmtRef = \l -> "#" <> TB.decimal l <> "#",
      fmtCons = \fmt ->
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
    { fmtStr = TB.fromText
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

decycle :: Object n -> IO Decycled
decycle obj = do
  table <- HT.new :: IO (BasicHashTable Loc DecycleStatus)
  nextLabel <- newIORef (0 :: Label)

  let loop Undef = pure DUndef
      loop Empty = pure DEmpty
      loop (Bool b) = pure (DBool b)
      loop (Num n) = pure (DNum n)
      loop (Str s) = pure (DStr s)
      loop (Sym s) = pure (DSym s)
      loop (Port _) = pure DPort
      loop (Syn _) = pure DSyn
      loop (Func _) = pure DFunc
      loop (Cont _) = pure DCont
      loop o@(Cons car cdr) =
        HT.lookup table (loc o) >>= \case
          Nothing -> do
            HT.insert table (loc o) InProgress
            d1 <- deref car >>= loop
            d2 <- deref cdr >>= loop
            HT.mutate table (loc o) \mres ->
              let d = case mres of
                    Just InProgress -> DCons Nothing d1 d2
                    Just (CycleDetected l) -> DCons (Just l) d1 d2
                    _ -> error "Unreachable"
               in (Just (Done d), d)
          Just InProgress -> do
            l <- readIORef nextLabel <* modifyIORef' nextLabel succ
            HT.insert table (loc o) (CycleDetected l)
            pure (DRef l)
          Just (CycleDetected l) -> pure (DRef l)
          Just (Done d) -> pure d

  loop obj

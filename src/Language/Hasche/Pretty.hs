module Language.Hasche.Pretty
  ( write,
    display,
    pretty,
    PrettyOption (..),
    defaultPrettyOption,
    Decycled (..),
    Label,
  )
where

import Control.Monad.IO.Class
import Data.HashTable.IO qualified as HT
import Data.IORef
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TB
import Language.Hasche.Eval
import Language.Hasche.Object

-- Prettyprinter for Object

write :: (MonadIO m) => Object (EvalT r m) -> EvalT r m T.Text
write = pretty writeOption

display :: (MonadIO m) => Object (EvalT r m) -> EvalT r m T.Text
display = pretty displayOption

data PrettyOption = PrettyOption
  { prettyUndef :: TB.Builder,
    prettyEmpty :: TB.Builder,
    prettyBool :: Bool -> TB.Builder,
    prettyNum :: Integer -> TB.Builder,
    prettyStr :: T.Text -> TB.Builder,
    prettySym :: T.Text -> TB.Builder,
    prettyPort :: TB.Builder,
    prettySyn :: TB.Builder,
    prettyFunc :: TB.Builder,
    prettyCont :: TB.Builder,
    prettyRef :: Label -> TB.Builder,
    prettyCons :: (Decycled -> TB.Builder) -> Maybe Label -> Decycled -> Decycled -> TB.Builder
  }

pretty :: (MonadIO m) => PrettyOption -> Object (EvalT r m) -> EvalT r m T.Text
pretty PrettyOption {..} =
  fmap (TL.toStrict . TB.toLazyText . format') . decycle
  where
    format' = \case
      DUndef -> prettyUndef
      DEmpty -> prettyEmpty
      DBool b -> prettyBool b
      DNum n -> prettyNum n
      DStr s -> prettyStr s
      DSym s -> prettySym s
      DPort -> prettyPort
      DSyn -> prettySyn
      DFunc -> prettyFunc
      DCont -> prettyCont
      DRef l -> prettyRef l
      DCons ml d1 d2 -> prettyCons format' ml d1 d2

writeOption :: PrettyOption
writeOption =
  PrettyOption
    { prettyUndef = "#<undef>",
      prettyEmpty = "()",
      prettyBool = \b -> if b then "#t" else "#f",
      prettyNum = TB.decimal,
      prettyStr = TB.fromString . show,
      prettySym = TB.fromText,
      prettyPort = "#<port>",
      prettySyn = "#<syntax>",
      prettyFunc = "#<procedure>",
      prettyCont = "#<continuation>",
      prettyRef = \l -> "#" <> TB.decimal l <> "#",
      prettyCons = \p ->
        let fmtCons Nothing (DSym "quote") (DCons Nothing d DEmpty) =
              "'" <> p d
            fmtCons Nothing (DSym "quasiquote") (DCons Nothing d DEmpty) =
              "`" <> p d
            fmtCons Nothing (DSym "unquote") (DCons Nothing d DEmpty) =
              "," <> p d
            fmtCons Nothing (DSym "unquote-splicing") (DCons Nothing d DEmpty) =
              ",@" <> p d
            fmtCons (Just l) car cdr =
              "#" <> TB.decimal l <> "=(" <> fmtList car cdr <> ")"
            fmtCons Nothing car cdr =
              "(" <> fmtList car cdr <> ")"

            fmtList d DEmpty = p d
            fmtList d1 (DCons Nothing d2 d3) = p d1 <> " " <> fmtList d2 d3
            fmtList d1 d2 = p d1 <> " . " <> p d2
         in fmtCons
    }

displayOption :: PrettyOption
displayOption = writeOption {prettyStr = TB.fromText}

defaultPrettyOption :: PrettyOption
defaultPrettyOption = displayOption

-- Decycling: replace circular reference with label

data Decycled
  = DUndef
  | DEmpty
  | DBool Bool
  | DNum Integer
  | DStr T.Text
  | DSym T.Text
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

decycle :: (MonadIO m) => Object (EvalT r m) -> EvalT r m Decycled
decycle obj = do
  table :: HT.BasicHashTable (ObjID (EvalT r m)) DecycleStatus <- liftIO HT.new
  nextLabel <- liftIO $ newIORef 0

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
      loop o@(Cons car cdr) = do
        let i = objID o
        liftIO (HT.lookup table i) >>= \case
          Nothing -> do
            liftIO $ HT.insert table i InProgress
            d1 <- deref car >>= loop
            d2 <- deref cdr >>= loop
            liftIO $ HT.mutate table i \mres ->
              let d = case mres of
                    Just InProgress -> DCons Nothing d1 d2
                    Just (CycleDetected lbl) -> DCons (Just lbl) d1 d2
                    _ -> error "impossible"
               in (Just (Done d), d)
          Just InProgress -> do
            lbl <- liftIO $ readIORef nextLabel <* modifyIORef' nextLabel succ
            liftIO $ HT.insert table i (CycleDetected lbl)
            pure (DRef lbl)
          Just (CycleDetected lbl) -> pure (DRef lbl)
          Just (Done d) -> pure d

  loop obj

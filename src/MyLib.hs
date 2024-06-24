{-# LANGUAGE GADTs #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2024 the ui authors
-- License     : Apache-2.0
module MyLib
  ( Scope,
    mark,
    Expr,
    run,
    Signal,
    signal,
    readS,
    writeS,
    modifyS,
    Component,
    memo,
    effect,
    runC,
    Html,
    compileHtml,
    runHtml,
    _component,
    _div,
    _text,
  )
where

import Control.Monad (ap)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)

data SignalData a = SignalData
  { valueSD :: a,
    subscribersSD :: [IORef Bool]
  }

newtype Signal a = Signal (IORef (SignalData a))

data Op m a where
  ExprO :: m a -> Op m a
  MarkO :: Bool -> Op m ()
  ReadO :: Signal a -> Op m a
  WriteO :: a -> Signal a -> Op m ()
  ModifyO :: (a -> a) -> Signal a -> Op m ()

data Scope m a where
  PureS :: a -> Scope m a
  OnceS :: Op m a -> Scope m a
  MapS :: (b -> a) -> Scope m b -> Scope m a
  BindS :: Scope m b -> (b -> Scope m a) -> Scope m a

instance Functor (Scope m) where
  fmap = MapS

instance Applicative (Scope m) where
  pure = PureS
  (<*>) sf sa = BindS sf (`fmap` sa)

instance Monad (Scope m) where
  (>>=) = BindS

instance (MonadIO m) => MonadIO (Scope m) where
  liftIO = OnceS . ExprO . liftIO

mark :: Bool -> Scope m ()
mark = OnceS . MarkO

readS :: Signal a -> Scope m a
readS = OnceS . ReadO

writeS :: a -> Signal a -> Scope m ()
writeS val s = OnceS $ WriteO val s

modifyS :: (a -> a) -> Signal a -> Scope m ()
modifyS val s = OnceS $ ModifyO val s

data Expr m a where
  PureE :: a -> Expr m a
  ScopeE :: Scope m a -> Maybe a -> IORef Bool -> Expr m a

instance Functor (Expr m) where
  fmap f (PureE a) = PureE $ f a
  fmap f (ScopeE s cached b) = ScopeE (fmap f s) (fmap f cached) b

instance Applicative (Expr m) where
  pure = PureE
  (<*>) = ap

-- TODO cache?
instance Monad (Expr m) where
  PureE a >>= f = f a
  ScopeE s _ ref >>= f = ScopeE (s >>= runExpr . f) Nothing ref
    where
      runExpr (PureE a) = return a
      runExpr (ScopeE s' _ _) = s'

compile :: (MonadIO m) => Scope m a -> m (Expr m a)
compile (PureS s) = return $ PureE s
compile s = do
  b <- liftIO $ newIORef False
  return $ ScopeE s Nothing b

run :: (MonadIO m) => Expr m a -> m (a, Expr m a)
run (PureE e) = return (e, PureE e)
run (ScopeE s cached b) = do
  case cached of
    Just cached' -> do
      b' <- liftIO $ readIORef b
      if b'
        then do
          x <- runScope s b
          return (x, ScopeE s (Just x) b)
        else return (cached', ScopeE s cached b)
    Nothing -> do
      out <- runScope s b
      return (out, ScopeE s (Just out) b)

runScope :: (MonadIO m) => Scope m a -> IORef Bool -> m a
runScope (PureS s) _ = return s
runScope (OnceS s) isReadyRef = runOp s isReadyRef
runScope (MapS f s) isReadyRef = f <$> runScope s isReadyRef
runScope (BindS s f) isReadyRef = do
  b <- runScope s isReadyRef
  runScope (f b) isReadyRef

runOp :: (MonadIO m) => Op m a -> IORef Bool -> m a
runOp (ExprO o) _ = o
runOp (MarkO b) isReadyRef = liftIO $ writeIORef isReadyRef b
runOp (ReadO (Signal s)) b = do
  liftIO $ modifyIORef s (\d -> d {subscribersSD = subscribersSD d ++ [b]})
  liftIO $ valueSD <$> readIORef s
runOp (WriteO val (Signal s)) _ = do
  d <- liftIO $ readIORef s
  liftIO $ mapM_ (`writeIORef` True) (subscribersSD d)
  liftIO $ modifyIORef s (\v -> v {valueSD = val})
runOp (ModifyO f (Signal s)) b = do
  liftIO $
    modifyIORef
      s
      ( \v ->
          v
            { valueSD = f $ valueSD v,
              subscribersSD = subscribersSD v ++ [b]
            }
      )
  d <- liftIO $ readIORef s
  liftIO $ mapM_ (`writeIORef` True) (subscribersSD d)

data ComponentOp m a where
  ExprCO :: m a -> ComponentOp m a
  SignalCO :: a -> ComponentOp m (Signal a)

runComponentOp :: (MonadIO m) => ComponentOp m a -> m a
runComponentOp (ExprCO e) = e
runComponentOp (SignalCO val) = do
  r <- liftIO $ newIORef SignalData {valueSD = val, subscribersSD = []}
  return $ Signal r

data Component m a where
  PureC :: a -> Component m a
  OnceC :: ComponentOp m a -> Component m a
  EffectC :: Scope m () -> Component m ()
  MemoC :: Scope m a -> Component m (Signal a)
  MapC :: (b -> a) -> Component m b -> Component m a
  BindC :: Component m b -> (b -> Component m a) -> Component m a

instance Functor (Component m) where
  fmap = MapC

instance Applicative (Component m) where
  pure = PureC
  (<*>) cf c = BindC cf (`fmap` c)

instance Monad (Component m) where
  (>>=) = BindC

instance (MonadIO m) => MonadIO (Component m) where
  liftIO = OnceC . ExprCO . liftIO

effect :: Scope m () -> Component m ()
effect = EffectC

memo :: Scope m a -> Component m (Signal a)
memo = MemoC

signal :: a -> Component m (Signal a)
signal = OnceC . SignalCO

runC :: (MonadIO m) => Component m a -> m (a, [Expr m ()])
runC (PureC a) = return (a, [])
runC (OnceC a) = do
  a' <- runComponentOp a
  return (a', [])
runC (EffectC s) = do
  e <- compile s
  return ((), [e])
runC (MemoC s) = do
  e <- compile s
  (a, _) <- run e
  let c = do
        sig <- signal a
        effect $ do
          a2 <- s
          writeS a2 sig
        return sig
  (sig, e3) <- runC c
  return (sig, e3)
runC (MapC f c) = do
  (b, exprs) <- runC c
  return (f b, exprs)
runC (BindC c f) = do
  (b, exprs) <- runC c
  (a, exprs') <- runC (f b)
  return (a, exprs ++ exprs')

data Html m
  = HtmlElement String [(String, Scope m String)] [Html m]
  | HtmlText (Scope m String)
  | HtmlComponent (Component m (Html m))

_component :: Component m (Html m) -> Html m
_component = HtmlComponent

_div :: [(String, Scope m String)] -> [Html m] -> Html m
_div = HtmlElement "div"

_text :: Scope m String -> Html m
_text = HtmlText

data HtmlExpr m
  = HtmlComponentExpr [Expr m ()] (HtmlExpr m)
  | HtmlElementExpr String [(String, Expr m String)] [HtmlExpr m]
  | HtmlTextExpr (Expr m String)

compileHtml :: (MonadIO m) => Html m -> m (HtmlExpr m)
compileHtml html = case html of
  HtmlComponent c -> do
    (html2, effects) <- runC c
    e <- compileHtml html2
    return $ HtmlComponentExpr effects e
  HtmlElement tag attrs children -> do
    attrs' <-
      mapM
        ( \(n, v) -> do
            v' <- compile v
            return (n, v')
        )
        attrs
    children' <- mapM compileHtml children
    return $ HtmlElementExpr tag attrs' children'
  HtmlText s -> fmap HtmlTextExpr (compile s)

runHtml :: (MonadIO m) => HtmlExpr m -> m (HtmlExpr m)
runHtml html = case html of
  HtmlComponentExpr effects content -> do
    effects2 <- mapM (fmap snd . run) effects
    content2 <- runHtml content
    return $ HtmlComponentExpr effects2 content2
  HtmlElementExpr tag attrs children -> do
    attrs2 <-
      mapM
        ( \(n, v) -> do
            (_, e) <- run v
            return (n, e)
        )
        attrs
    children2 <- mapM runHtml children
    return $ HtmlElementExpr tag attrs2 children2
  HtmlTextExpr e -> do
    (content, e2) <- run e
    return $ HtmlTextExpr e2
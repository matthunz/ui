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
    liftIOC,
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

compile :: Scope IO a -> IO (Expr IO a)
compile (PureS s) = return $ PureE s
compile s = do
  b <- newIORef False
  return $ ScopeE s Nothing b

run :: Expr IO a -> IO (a, Expr IO a)
run (PureE e) = return (e, PureE e)
run (ScopeE s cached b) = do
  case cached of
    Just cached' -> do
      b' <- readIORef b
      if b'
        then do
          x <- runScope s b
          return (x, ScopeE s (Just x) b)
        else return (cached', ScopeE s cached b)
    Nothing -> do
      out <- runScope s b
      return (out, ScopeE s (Just out) b)

runScope :: Scope IO a -> IORef Bool -> IO a
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

data ComponentOp a where
  ExprCO :: IO a -> ComponentOp a
  SignalCO :: a -> ComponentOp (Signal a)

runComponentOp :: ComponentOp a -> IO a
runComponentOp (ExprCO io) = io
runComponentOp (SignalCO val) = do
  r <- newIORef SignalData {valueSD = val, subscribersSD = []}
  return $ Signal r

data Component a where
  PureC :: a -> Component a
  OnceC :: ComponentOp a -> Component a
  EffectC :: Scope IO () -> Component ()
  MemoC :: Scope IO a -> Component (Signal a)
  MapC :: (b -> a) -> Component b -> Component a
  BindC :: Component b -> (b -> Component a) -> Component a

instance Functor Component where
  fmap = MapC

instance Applicative Component where
  pure = PureC
  (<*>) cf c = BindC cf (`fmap` c)

instance Monad Component where
  (>>=) = BindC

liftIOC :: IO a -> Component a
liftIOC = OnceC . ExprCO

effect :: Scope IO () -> Component ()
effect = EffectC

memo :: Scope IO a -> Component (Signal a)
memo = MemoC

signal :: a -> Component (Signal a)
signal = OnceC . SignalCO

runC :: Component a -> IO (a, [Expr IO ()])
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

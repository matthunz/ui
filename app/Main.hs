{-# LANGUAGE GADTs #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2024 the ui authors
-- License     : Apache-2.0
module Main where

import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)

main :: IO ()
main = do
  (_, effects) <- runC app
  exprs <- mapM run effects
  xs1 <- mapM (\(_, expr) -> run expr) exprs
  mapM_ (\(_, expr) -> run expr) xs1

app :: Component ()
app = do
  x <- signal (0 :: Int)

  effect $ do
    n <- readS x
    liftIO $ print n

  effect $ do
    n <- readS x
    writeS (n + 1) x

data SignalData a = SignalData
  { valueSD :: a,
    subscribersSD :: [IORef Bool]
  }

newtype Signal a = Signal
  { unSignal :: IORef (SignalData a)
  }

data Op a where
  ExprO :: IO a -> Op a
  MarkO :: Bool -> Op ()
  ReadO :: Signal a -> Op a
  WriteO :: a -> Signal a -> Op ()

data Scope a where
  PureS :: a -> Scope a
  OnceS :: Op a -> Scope a
  MapS :: (b -> a) -> Scope b -> Scope a
  BindS :: Scope b -> (b -> Scope a) -> Scope a

instance Functor Scope where
  fmap = MapS

instance Applicative Scope where
  pure = PureS
  (<*>) sf sa = BindS sf (`fmap` sa)

instance Monad Scope where
  (>>=) = BindS

liftIO :: IO a -> Scope a
liftIO = OnceS . ExprO

mark :: Bool -> Scope ()
mark = OnceS . MarkO

readS :: Signal a -> Scope a
readS = OnceS . ReadO

writeS :: a -> Signal a -> Scope ()
writeS val s = OnceS $ WriteO val s

data Expr a where
  PureE :: a -> Expr a
  ScopeE :: Scope a -> Maybe a -> IORef Bool -> Expr a

compile :: Scope a -> IO (Expr a)
compile (PureS s) = return $ PureE s
compile s = do
  b <- newIORef False
  return $ ScopeE s Nothing b

run :: Expr a -> IO (a, Expr a)
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

runScope :: Scope a -> IORef Bool -> IO a
runScope (PureS s) _ = return s
runScope (OnceS s) isReadyRef = runOp s isReadyRef
runScope (MapS f s) isReadyRef = f <$> runScope s isReadyRef
runScope (BindS s f) isReadyRef = do
  b <- runScope s isReadyRef
  runScope (f b) isReadyRef

runOp :: Op a -> IORef Bool -> IO a
runOp (ExprO o) _ = o
runOp (MarkO b) isReadyRef = writeIORef isReadyRef b
runOp (ReadO (Signal s)) b = do
  modifyIORef s (\d -> d {subscribersSD = subscribersSD d ++ [b]})
  valueSD <$> readIORef s
runOp (WriteO val (Signal s)) _ = do
  d <- readIORef s
  mapM_ (`writeIORef` True) (subscribersSD d)
  modifyIORef s (\v -> v {valueSD = val})

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
  EffectC :: Scope () -> Component ()
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

effect :: Scope () -> Component ()
effect = EffectC

signal :: a -> Component (Signal a)
signal = OnceC . SignalCO

runC :: Component a -> IO (a, [Expr ()])
runC (PureC a) = return (a, [])
runC (OnceC a) = do
  a' <- runComponentOp a
  return (a', [])
runC (EffectC s) = do
  e <- compile s
  return ((), [e])
runC (MapC f c) = do
  (b, exprs) <- runC c
  return (f  b, exprs)
runC (BindC c f) = do
  (b, exprs) <- runC c
  (a, exprs') <- runC (f b)
  return (a, exprs ++ exprs')

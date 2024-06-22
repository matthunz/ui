{-# LANGUAGE GADTs #-}

module Main where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

main :: IO ()
main = do
  expr1 <- compile app
  ((), expr2) <- run expr1
  _ <- run expr2
  return ()

app :: Scope ()
app = do
  mark True
  liftIO $ print "A"

data Op a where
  ExprO :: IO a -> Op a
  MarkO :: Bool -> Op ()

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
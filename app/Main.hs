{-# LANGUAGE GADTs #-}

module Main where

import qualified MyLib (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc

data Scope a where
  PureS :: a -> Scope a
  OnceS :: IO a -> Scope a
  MapS :: (b -> a) -> Scope b -> Scope a
  BindS :: Scope b -> (b -> Scope a) -> Scope a

instance Functor Scope where
  fmap = MapS

instance Applicative Scope where
  pure = PureS
  (<*>) sf sa = BindS sf (`fmap` sa)

instance Monad Scope where
  (>>=) = BindS
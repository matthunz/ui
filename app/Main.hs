{-# LANGUAGE GADTs #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2024 the ui authors
-- License     : Apache-2.0
module Main where

import MyLib

main :: IO ()
main = do
  (_, effects) <- runC app
  exprs <- mapM run effects
  xs1 <- mapM (\(_, expr) -> run expr) exprs
  mapM_ (\(_, expr) -> run expr) xs1

app :: Component ()
app = do
  x <- signal (0 :: Int)
  x' <- memo $ do
    n <- readS x
    return $ n * 2

  effect $ do
    n <- readS x'
    liftIO $ print n

  effect $ modifyS (+ 1) x

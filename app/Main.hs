{-# LANGUAGE GADTs #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2024 the ui authors
-- License     : Apache-2.0
module Main where

import Control.Monad.IO.Class (liftIO)
import MyLib

main :: IO ()
main = do
  html <- compileHtml app
  out <- runHtml html
  return ()

app :: Html IO
app = _component $ do
  x <- signal (0 :: Int)

  s <- memo $ do
    x' <- readS x
    return $ x' + 1

  effect $ writeS 1 x

  return $
    _div
      []
      [ _text $ do
          x' <- readS x
          s' <- readS s
          return $ show x' ++ "+ 1 =" ++ show s'
      ]

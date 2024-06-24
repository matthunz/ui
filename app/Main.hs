{-# LANGUAGE GADTs #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2024 the ui authors
-- License     : Apache-2.0
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Functor
import Html
import MyLib

main :: IO ()
main = do
  html <- compileHtml app

  (out, html2) <- runHtml html
  print out

  (out, _) <- runHtml html2
  print out

app :: Html IO
app = _component $ do
  x <- signal (0 :: Int)

  s <- memo $ readS x <&> (+ 1)

  effect $ writeS 1 x

  return $
    _div
      [("class", pure "main")]
      [ _text $ do
          x' <- readS x
          s' <- readS s
          return $ show x' ++ " + 1 = " ++ show s'
      ]

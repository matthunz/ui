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
  html <- compileHtml counter

  (out, html2) <- runHtml html
  print out

  (out, _) <- runHtml html2
  print out

counter :: Html IO
counter = _component $ do
  x <- signal (0 :: Int)

  return $
    _div
      [("class", _attr $ pure "main")]
      [ _text $ do
          x' <- readS x
          return $ "High five count: " ++ show x',
        _div
          [_on "click" (modifySIO (+ 1) x)]
          [_text $ pure "Up high!"],
        _div
          [_on "click" (modifySIO ((-) 1) x)]
          [_text $ pure "Down low!"]
      ]

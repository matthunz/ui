{-# LANGUAGE GADTs #-}

-- |
-- Module      : Html
-- Copyright   : (c) 2024 the ui authors
-- License     : Apache-2.0
module Html
  ( Html,
    compileHtml,
    runHtml,
    _component,
    _div,
    _text,
  )
where

import Control.Monad (mapAndUnzipM)
import Control.Monad.IO.Class (MonadIO)
import MyLib

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

runHtml :: (MonadIO m) => HtmlExpr m -> m (HtmlData, HtmlExpr m)
runHtml html = case html of
  HtmlComponentExpr effects content -> do
    (contentData, content2) <- runHtml content
    effects2 <- mapM (fmap snd . run) effects
    return (contentData, HtmlComponentExpr effects2 content2)
  HtmlElementExpr tag attrs children -> do
    attrs2 <-
      mapM
        ( \(n, v) -> do
            (v', e) <- run v
            return (n, v', e)
        )
        attrs
    (childrenData, children2) <- mapAndUnzipM runHtml children

    return
      ( HtmlElementData tag (map (\(n, v, _) -> (n, v)) attrs2) childrenData,
        HtmlElementExpr tag (map (\(_, v, e) -> (v, e)) attrs2) children2
      )
  HtmlTextExpr e -> do
    (content, e2) <- run e
    return (HtmlTextData content, HtmlTextExpr e2)

data HtmlData
  = HtmlTextData String
  | HtmlElementData String [(String, String)] [HtmlData]

instance Show HtmlData where
  show (HtmlTextData s) = s
  show (HtmlElementData tag attrs children) =
    "<"
      ++ tag
      ++ concatMap (\(n, v) -> " " ++ n ++ "=" ++ show v) attrs
      ++ ">"
      ++ concatMap show children
      ++ "</"
      ++ tag
      ++ ">"
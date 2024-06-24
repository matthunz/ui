{-# LANGUAGE GADTs #-}

-- |
-- Module      : Html
-- Copyright   : (c) 2024 the ui authors
-- License     : Apache-2.0
module Html
  ( Html,
    compileHtml,
    runHtml,
    _attr,
    _on,
    _component,
    _div,
    _text,
  )
where

import Control.Monad (mapAndUnzipM)
import Control.Monad.IO.Class (MonadIO)
import MyLib

data HtmlAttribute m = HtmlTextAttribute (Scope m String) | Handler (m ())

_attr :: Scope m String -> HtmlAttribute m
_attr = HtmlTextAttribute

_on :: m () -> HtmlAttribute m
_on = Handler

data Html m
  = HtmlElement String [(String, HtmlAttribute m)] [Html m]
  | HtmlText (Scope m String)
  | HtmlComponent (Component m (Html m))

_component :: Component m (Html m) -> Html m
_component = HtmlComponent

_div :: [(String, HtmlAttribute m)] -> [Html m] -> Html m
_div = HtmlElement "div"

_text :: Scope m String -> Html m
_text = HtmlText

data HtmlAttributeExpr m = HtmlTextAttributeExpr (Expr m String) | HandlerExpr (m ())

data HtmlExpr m
  = HtmlComponentExpr [Expr m ()] (HtmlExpr m)
  | HtmlElementExpr String [(String, HtmlAttributeExpr m)] [HtmlExpr m]
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
            expr <- case v of
              HtmlTextAttribute s -> do
                s' <- compile s
                return $ HtmlTextAttributeExpr s'
              Handler f -> return $ HandlerExpr f
            return (n, expr)
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
            (d, expr) <- case v of
              HtmlTextAttributeExpr e -> do
                (n2, e2) <- run e
                return (HtmlTextAttributeData n2, HtmlTextAttributeExpr e2)
              HandlerExpr f -> return (HandlerData, HandlerExpr f)
            return (n, d, expr)
        )
        attrs
    (childrenData, children2) <- mapAndUnzipM runHtml children

    return
      ( HtmlElementData tag (map (\(n, v, _) -> (n, v)) attrs2) childrenData,
        HtmlElementExpr tag (map (\(n, _, e) -> (n, e)) attrs2) children2
      )
  HtmlTextExpr e -> do
    (content, e2) <- run e
    return (HtmlTextData content, HtmlTextExpr e2)

data HtmlAttributeData = HtmlTextAttributeData String | HandlerData

instance Show HtmlAttributeData where
  show (HtmlTextAttributeData s) = show s
  show HandlerData = "<handler>"

data HtmlData
  = HtmlTextData String
  | HtmlElementData String [(String, HtmlAttributeData)] [HtmlData]

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
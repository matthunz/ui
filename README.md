```hs
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
```
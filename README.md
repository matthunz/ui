```hs
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
```
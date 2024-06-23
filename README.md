```hs
app :: Component ()
app = do
  x <- signal (0 :: Int)
  x' <- memo $ do
    n <- readS x
    return $ n * 2

  effect $ do
    n <- readS x'
    liftIO $ print n

  effect $ do
    n <- readS x
    writeS (n + 1) x
```
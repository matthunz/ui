```hs
app :: Component ()
app = do
  x <- signal (0 :: Int)

  effect $ do
    n <- readS x
    liftIO $ print n

  effect $ do
    n <- readS x
    writeS (n + 1) x
```
{-# LANGUAGE NoImplicitPrelude #-}

import RIO
import Test.Hspec
import Control.Monad

data App = App
  { appCounter :: !(TVar Int)
  }

expensiveComputation :: RIO App Int
expensiveComputation = do
  app <- ask
  -- I'm not actually an expensive computation, but I play one on TV
  delayVar <- registerDelay 1000000
  atomically $ do
    readTVar delayVar >>= checkSTM
    let var = appCounter app
    modifyTVar' var (+ 1)
    readTVar var

main :: IO ()
main = hspec $
  it "works" $ do
    app <- App <$> newTVarIO 0
    res <- runRIO app $ runConcurrently $ replicateM 10 (Concurrently expensiveComputation)
    sum res `shouldBe` sum [1 .. 10]
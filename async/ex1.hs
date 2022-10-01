{-# LANGUAGE OverloadedStrings #-}
-- #!/usr/bin/env cabal
{- cabal:
build-depends:
            , async
            , base   ^>=4.14.3.0
            , mtl
            , text
            , monad-control
            , say
            , stm
            , unliftio
            , rio
            , hspec

ghc-options:      -threaded
-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Monad (replicateM)
import RIO
import Test.Hspec
import Say

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

-- main :: IO ()
-- main = hspec $
--   it "works" $ do
--     app <- App <$> newTVarIO 0
--     res <- runRIO app $ runConcurrently . sequenceA_ $ replicate 10 $ Concurrently expensiveComputation
--     res `shouldBe` sum [1 .. 10]
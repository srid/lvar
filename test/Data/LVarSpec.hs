module Data.LVarSpec where

import Test.Hspec
import Control.Concurrent.Async (async, wait, race)
import Control.Concurrent (threadDelay)
import Data.LVar

spec :: Spec
spec = do
  describe "Data.LVar" $ do
    describe "basic operations" $ do
      it "creates and gets initial value" $ do
        lvar <- new (42 :: Int)
        get lvar `shouldReturn` 42

      it "sets new values" $ do
        lvar <- new (1 :: Int)
        set lvar 2
        get lvar `shouldReturn` 2

      it "modifies values" $ do
        lvar <- new (10 :: Int)
        modify lvar (* 2)
        get lvar `shouldReturn` 20

      it "works with empty LVars" $ do
        lvar <- empty :: IO (LVar Int)
        set lvar 100
        get lvar `shouldReturn` 100

    describe "listenNext" $ do
      it "receives updates from set" $ do
        lvar <- new (1 :: Int)
        listener <- async $ listenNext lvar
        modifier <- async $ do
          threadDelay 1000
          set lvar 2
        result <- race (threadDelay 1_000_000) (wait listener)
        wait modifier
        result `shouldBe` Right 2

      it "receives updates from modify" $ do
        lvar <- new (10 :: Int)
        listener <- async $ listenNext lvar
        modifier <- async $ do
          threadDelay 1000
          modify lvar (* 3)
        result <- race (threadDelay 1_000_000) (wait listener)
        wait modifier
        result `shouldBe` Right 30

      it "works with empty LVars" $ do
        lvar <- empty :: IO (LVar Int)
        listener <- async $ listenNext lvar
        modifier <- async $ do
          threadDelay 1000
          set lvar 42
        result <- race (threadDelay 1_000_000) (wait listener)
        wait modifier
        result `shouldBe` Right 42

      it "multiple listeners get same update" $ do
        lvar <- new (1 :: Int)
        listener1 <- async $ listenNext lvar
        listener2 <- async $ listenNext lvar
        modifier <- async $ do
          threadDelay 1000
          set lvar 999
        result1 <- race (threadDelay 1_000_000) (wait listener1)
        result2 <- race (threadDelay 1_000_000) (wait listener2)
        wait modifier
        result1 `shouldBe` Right 999
        result2 `shouldBe` Right 999

      it "gets first update when multiple sets occur" $ do
        lvar <- new (1 :: Int)
        listener1 <- async $ listenNext lvar
        modifier <- async $ do
          threadDelay 1000
          set lvar 2
          threadDelay 1000
          set lvar 3
          threadDelay 1000
          set lvar 4
        result1 <- race (threadDelay 1_000_000) (wait listener1)
        result1 `shouldBe` Right 2
        
        listener2 <- async $ listenNext lvar
        result2 <- race (threadDelay 1_000_000) (wait listener2)
        result2 `shouldBe` Right 3
        
        listener3 <- async $ listenNext lvar
        result3 <- race (threadDelay 1_000_000) (wait listener3)
        wait modifier
        result3 `shouldBe` Right 4

    describe "concurrency" $ do
      it "handles concurrent modifications" $ do
        lvar <- new (0 :: Int)
        modifiers <- mapM (\_ -> async $ modify lvar (+ 1)) [1..10]
        mapM_ wait modifiers
        get lvar `shouldReturn` 10

      it "notifies concurrent listeners" $ do
        lvar <- new (0 :: Int)
        listeners <- mapM (\_ -> async $ listenNext lvar) [1..5]
        modifier <- async $ do
          threadDelay 1000
          set lvar 123
        results <- mapM (\l -> race (threadDelay 1_000_000) (wait l)) listeners
        wait modifier
        results `shouldBe` replicate 5 (Right 123)
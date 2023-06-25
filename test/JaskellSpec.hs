{-# LANGUAGE BlockArguments #-}
module JaskellSpec (spec) where

import Test.Hspec (Spec, it, describe, shouldBe)
import Control.Category ((>>>))
import Jaskell

spec :: Spec
spec = do
  describe "Jaskell.push" do
    it "pushes values onto the stack" do
      (push 0 >>> push 'a' >>> push ('b', 'c')) () `shouldBe` ((((), 0 :: Int), 'a'), ('b', 'c'))
    
  describe "Jaskell.liftS" do
    it "applies a function to the top value" do
      (push 5 >>> liftS (1+)) () `shouldBe` ((), 6 :: Int) 
  
  describe "Jaskell.liftS2" do
    it "applies a function to the top two values" do
      (push "bob" >>> push "cat" >>> liftS2 (++)) () `shouldBe` ((), "bobcat")
  
  describe "Jaskell.pushM" do
    it "can read input" do
      s <- runK (pushM getLine)
      s `shouldBe` ((), "hello")

{-
  ( push, liftS, liftS2, pushM, popM, liftSM
  , run, runOn, runK, runKOn
  )
-}
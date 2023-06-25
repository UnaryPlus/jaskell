{-# LANGUAGE BlockArguments #-}
module JaskellSpec (spec) where

import Test.Hspec (Spec, it, describe, shouldBe)
import Control.Category ((>>>))
import System.Directory (removeFile)
import Jaskell ( liftS, liftS2, liftSM, popM, push, pushM, runK )

testPutStr :: String -> IO ()
testPutStr = writeFile "/dev/tty"

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
      testPutStr "Type 'hello':\n"
      s <- runK (pushM getLine)
      s `shouldBe` ((), "hello")
  
  describe "Jaskell.popM" do
    it "can output text" do
      runK (push "foo" >>> push "\n" >>> liftS2 (++) >>> popM testPutStr)
      testPutStr "Was the word 'foo' just printed? (y/n) "
      res <- getLine
      res `shouldBe` "y"
  
  describe "Jaskell.liftSM" do
    it "can read from files" do
      writeFile "tmp-input.txt" "lorem ipsum"
      s <- runK (push "tmp-input.txt" >>> liftSM readFile)
      s `shouldBe` ((), "lorem ipsum")
      removeFile "tmp-input.txt"
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}
module Jaskell.QuoteSpec (spec) where

import Test.Hspec (Spec, it, describe, shouldBe)
import Text.Megaparsec (parse)
import Jaskell (run)
import Jaskell.Quote

import qualified Data.List

spec :: Spec
spec = do
  describe "Jaskell.Quote.parseName" do
    it "parses function names" do
      parse parseName "" "putStrLn" `shouldBe` Right (Fun [] "putStrLn")
      parse parseName "" "array_2_list'" `shouldBe` Right (Fun [] "array_2_list'")
    it "parses constructor names" do
      parse parseName "" "LiftSM" `shouldBe` Right (Ctor [] "LiftSM")
      parse parseName "" "A_'2" `shouldBe` Right (Ctor [] "A_'2")
    it "parses qualified names" do
      parse parseName "" "Prelude.map" `shouldBe` Right (Fun ["Prelude"] "map")
      parse parseName "" "M1.M2.C'tor" `shouldBe` Right (Ctor ["M1", "M2"] "C'tor")
  
  describe "Jaskell.Quote.parseLiteral" do
    it "parses character literals" do
      parse parseLiteral "" "'a'" `shouldBe` Right (Char 'a')
      parse parseLiteral "" "'\\''" `shouldBe` Right (Char '\'')
      parse parseLiteral "" "'\\x00FC'" `shouldBe` Right (Char '\x00FC')
    it "parses string literals" do
      parse parseLiteral "" "\"L2R\"" `shouldBe` Right (String "L2R")
      parse parseLiteral "" "\"\\x0254\\\"\"" `shouldBe` Right (String "\x0254\"")
    it "parses integer literals" do
      parse parseLiteral "" "144" `shouldBe` Right (Integer 144)
      parse parseLiteral "" "-1" `shouldBe` Right (Integer (-1))
    it "parses floating point literals" do
      parse parseLiteral "" "3.14" `shouldBe` Right (Double 3.14)
      parse parseLiteral "" "-1e6" `shouldBe` Right (Double (-1000000))
    it "parses empty tuples" do
      parse parseLiteral "" "()" `shouldBe` Right Unit
  
  describe "Jaskell.Quote.parseCommand" do
    it "parses names with prefixes" do
      parse parseCommand "" "pop" `shouldBe` Right (Name Bare (Fun [] "pop"))
      parse parseCommand "" "DEFINE" `shouldBe` Right (Name Bare (Ctor [] "DEFINE")) 
      parse parseCommand "" "$Just" `shouldBe` Right (Name LiftS (Ctor [] "Just"))
      parse parseCommand "" "#Data.List.elem" `shouldBe` Right (Name LiftS2 (Fun ["Data", "List"] "elem"))
      parse parseCommand "" "?getLine" `shouldBe` Right (Name PushM (Fun [] "getLine"))
      parse parseCommand "" "!putStrLn" `shouldBe` Right (Name PopM (Fun [] "putStrLn"))
      parse parseCommand "" "&IO.readFile" `shouldBe` Right (Name LiftSM (Fun ["IO"] "readFile"))
    it "parses tuples" do
      parse parseCommand "" "( 0 , () ) " `shouldBe` Right (Tup (Expr [Lit (Integer 0)]) (Expr [Lit Unit]))
      parse parseCommand "" "(0(),((),0))" `shouldBe` Right (Tup (Expr [Lit (Integer 0), Lit Unit]) (Expr [Tup (Expr [Lit Unit]) (Expr [Lit (Integer 0)])]))
    it "parses lists" do
      parse parseCommand "" "[]" `shouldBe` Right (List [])
      parse parseCommand "" "[ 12, 0 1 pop ,'a' ] " `shouldBe` Right (List [Expr [Lit (Integer 12)], Expr [Lit (Integer 0), Lit (Integer 1), Name Bare (Fun [] "pop")], Expr [Lit (Char 'a')]])
    it "parses operators" do
      parse parseCommand "" "=@+&" `shouldBe` Right (Op "=@+&")
      parse parseCommand "" "-" `shouldBe` Right (Op "-")
      parse parseCommand "" "-1" `shouldBe` Right (Lit (Integer (-1))) 
    it "parses quoted expressions" do
      parse parseCommand "" "{ 2 dup * }" `shouldBe` Right (Quote (Just (Expr [Lit (Integer 2), Name Bare (Fun [] "dup"), Op "*"])))
  
  describe "Jaskell.quote.parseProgram" do
    it "parses expressions" do
      parse parseProgram "" "4 3 + 'a' #replicate " `shouldBe` Right (Program [] (Expr [Lit (Integer 4), Lit (Integer 3), Op "+", Lit (Char 'a'), Name LiftS2 (Fun [] "replicate")]))
    it "parses definitions" do
      parse parseProgram "" "DEF sq = dup * ; DEF inc = -1 - ; 4 sq inc" `shouldBe` Right (Program [("sq", Expr [Name Bare (Fun [] "dup"), Op "*"]), ("inc", Expr [Lit (Integer (-1)), Op "-"])] (Expr [Lit (Integer 4), Name Bare (Fun [] "sq"), Name Bare (Fun [] "inc")])) 
  
  describe "Jaskell.quote.jsl" do
    it "converts qualified module names properly" do
      run [jsl| "a" $Data.List.head |] `shouldBe` ((), 'a')
    -- TODO: add more tests?




{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
module Jaskell.PreludeSpec (spec) where

import Test.Hspec (Spec, it, describe, shouldBe)
import Jaskell.Prelude
import Jaskell.Quote (jsl)
import Jaskell (run, runOn)

spec :: Spec
spec = do
  describe "Jaskell.Prelude.stack" do
    it "works" do
      run [jsl| 'a' 'b' stack |] `shouldBe` ((((), 'a'), 'b'), (((), 'a'), 'b')) 
  
  describe "Jaskell.Prelude.unstack" do
    it "works" do
      run [jsl| 'x' ((), 'a') unstack 'b' |] `shouldBe` (((), 'a'), 'b')

  describe "Jaskell.Prelude.newstack" do
    it "works" do
      run [jsl| 'a' 'b' newstack |] `shouldBe` () 
    
  describe "Jaskell.Prelude.pop" do
    it "works" do
      run [jsl| 'a' 'b' pop |] `shouldBe` ((), 'a')
  
  describe "Jaskell.Prelude.dup" do
    it "works" do
      run [jsl| 'a' 'b' dup |] `shouldBe` ((((), 'a'), 'b'), 'b')

  describe "Jaskell.Prelude.swap" do
    it "works" do
      run [jsl| 'a' 'b' swap |] `shouldBe` (((), 'b'), 'a')

  describe "Jaskell.Prelude.popd" do
    it "works" do
      run [jsl| 'a' 'b' popd |] `shouldBe` ((), 'b')
  
  describe "Jaskell.Prelude.pop2" do
    it "works" do
      run [jsl| 'a' 'b' 'c' pop2 |] `shouldBe` ((), 'a')
  
  describe "Jaskell.Prelude.dupd" do
    it "works" do
      run [jsl| 'a' 'b' dupd |] `shouldBe` ((((), 'a'), 'a'), 'b')
  
  describe "Jaskell.Prelude.dup2" do
    it "works" do
      run [jsl| 'a' 'b' dup2 |] `shouldBe` (((((), 'a'), 'b'), 'a'), 'b')
  
  describe "Jaskell.Prelude.swapd" do
    it "works" do
      run [jsl| 'a' 'b' 'c' swapd |] `shouldBe` ((((), 'b'), 'a'), 'c')
  
  describe "Jaskell.Prelude.rollup" do
    it "works" do
      run [jsl| 'a' 'b' 'c' rollup |] `shouldBe` ((((), 'c'), 'a'), 'b') 
  
  describe "Jaskell.Prelude.rolldown" do
    it "works" do
      run [jsl| 'a' 'b' 'c' rolldown |] `shouldBe` ((((), 'b'), 'c'), 'a')

  describe "Jaskell.Prelude.choice" do
    it "works" do
      run [jsl| True 'a' 'b' choice |] `shouldBe` ((), 'a') 
      run [jsl| False 'a' 'b' choice |] `shouldBe` ((), 'b') 
  
  describe "Jaskell.Prelude.select" do
    it "works" do
      run [jsl| 'a' [ ('a', 1), ('b', 2) ] -1 select |] `shouldBe` ((), 1 :: Integer)
      run [jsl| 'b' [ ('a', 1), ('b', 2) ] -1 select |] `shouldBe` ((), 2 :: Integer)
      run [jsl| 'c' [ ('a', 1), ('b', 2) ] -1 select |] `shouldBe` ((), -1 :: Integer)
  
  describe "Jaskell.Prelude.pair" do
    it "works" do
      run [jsl| 'a' 'b' pair |] `shouldBe` ((), ('a', 'b'))
  
  describe "Jaskell.Prelude.unpair" do
    it "works" do
      run [jsl| ('a', 'b') unpair |] `shouldBe` (((), 'a'), 'b')
  
  describe "cons" do
    it "works" do
      run [jsl| 0 [1,2,3] cons |] `shouldBe` ((), [0,1,2,3 :: Integer])
  
  describe "swons" do
    it "works" do
      run [jsl| [1,2,3] 0 swons |] `shouldBe` ((), [0,1,2,3 :: Integer])
  
  describe "conjoin" do
    it "works" do
      let test = [jsl| { 3 > } { 5 < } conjoin i |]
      runOn ((), 3) test `shouldBe` (((), 3 :: Integer), False)
      runOn ((), 4) test `shouldBe` (((), 4 :: Integer), True)
      runOn ((), 5) test `shouldBe` (((), 5 :: Integer), False)
  
  describe "disjoin" do
    it "works" do
      let test = [jsl| { 7 <= } { 9 >= } disjoin i |]
      runOn ((), 7) test `shouldBe` (((), 7 :: Integer), True)
      runOn ((), 8) test `shouldBe` (((), 8 :: Integer), False)
      runOn ((), 9) test `shouldBe` (((), 9 :: Integer), True)
  
  describe "i" do
    it "works" do
      run [jsl| 'a' 'b' { pop 'c' } i |] `shouldBe` (((), 'a'), 'c')
  
  describe "comp" do
    it "works" do
      run [jsl| 'a' 'b' { pop } { 'c' } comp i |] `shouldBe` (((), 'a'), 'c')
    
  describe "consQ" do
    it "works" do
      run [jsl| 'a' 'b' { pop } { { 'c' } comp } consQ i i |] `shouldBe` (((), 'a'), 'c')
  
  describe "swonsQ" do
    it "works" do
      run [jsl| 'a' 'b' { { 'c' } comp i } { pop }  swonsQ i |] `shouldBe` (((), 'a'), 'c')

  describe "nullary" do
    it "works" do
      run [jsl| 5 7 { + } nullary |] `shouldBe` ((((), 5), 7), 12 :: Integer)

  describe "dip" do
    it "works" do
      run [jsl| 'a' 'b' 'c' { newstack } dip |] `shouldBe`

{-
  ( stack, unstack, newstack
  , pop, dup, swap, popd , pop2 , dupd, dup2, swapd, rollup, rolldown
  , choice, select
  , pair, unpair
  , cons, swons
  , conjoin, disjoin
  , i, comp
  , consQ, swonsQ
  , nullary, dip, dipd, dipdd
  , app1, app2, app3, cleave
  , ifte, whiledo
  , tailrec, linrec, binrec, natrec, listrec
  , cond, condlinrec
  , branch, times, infra
  , step, step2, map, mapS, filter, filterS, split, splitS
  , any, all, zipwith, zipwithS
  )
-}

{-
  describe "" do
    it "works" do
      run [jsl| |] `shouldBe`
-}
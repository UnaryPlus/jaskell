{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
module Jaskell.PreludeSpec (spec) where

import qualified Prelude
import Prelude hiding (map, filter, any, all, zipWith)

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
      run [jsl| 'a' [ ('a', 1), ('b', 2) ] -1 select |] `shouldBe` ((), 1 :: Int)
      run [jsl| 'b' [ ('a', 1), ('b', 2) ] -1 select |] `shouldBe` ((), 2 :: Int)
      run [jsl| 'c' [ ('a', 1), ('b', 2) ] -1 select |] `shouldBe` ((), -1 :: Int)
  
  describe "Jaskell.Prelude.pair" do
    it "works" do
      run [jsl| 'a' 'b' pair |] `shouldBe` ((), ('a', 'b'))
  
  describe "Jaskell.Prelude.unpair" do
    it "works" do
      run [jsl| ('a', 'b') unpair |] `shouldBe` (((), 'a'), 'b')
  
  describe "cons" do
    it "works" do
      run [jsl| 0 [1,2,3] cons |] `shouldBe` ((), [0,1,2,3 :: Int])
  
  describe "swons" do
    it "works" do
      run [jsl| [1,2,3] 0 swons |] `shouldBe` ((), [0,1,2,3 :: Int])
  
  describe "conjoin" do
    it "works" do
      let test = [jsl| { 3 > } { 5 < } conjoin i |]
      runOn ((), 3) test `shouldBe` (((), 3 :: Int), False)
      runOn ((), 4) test `shouldBe` (((), 4 :: Int), True)
      runOn ((), 5) test `shouldBe` (((), 5 :: Int), False)
  
  describe "disjoin" do
    it "works" do
      let test = [jsl| { 7 <= } { 9 >= } disjoin i |]
      runOn ((), 7) test `shouldBe` (((), 7 :: Int), True)
      runOn ((), 8) test `shouldBe` (((), 8 :: Int), False)
      runOn ((), 9) test `shouldBe` (((), 9 :: Int), True)
  
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
      run [jsl| 5 7 { + } nullary |] `shouldBe` ((((), 5), 7), 12 :: Int)

  describe "dip" do
    it "works" do
      run [jsl| 'a' 'b' 'c' { newstack } dip |] `shouldBe` ((), 'c')
  
  describe "dipd" do
    it "works" do
      run [jsl| 'a' 'b' 'c' 'd' { swap } dipd |] `shouldBe` (((((), 'b'), 'a'), 'c'), 'd')
  
  describe "dipdd" do
    it "works" do
      run [jsl| 'a' 'b' 'c' 'd' { pop 'x' } dipdd |] `shouldBe` (((((), 'x'), 'b'), 'c'), 'd')
  
  describe "app1" do
    it "works" do
      run [jsl| 'a' 'b' { pop } app1 |] `shouldBe` (((), 'a'), 'a')
  
  describe "app2" do
    it "works" do
      run [jsl| 4 5 { dup * } app2 |] `shouldBe` (((), 16), 25 :: Int)
  
  describe "app3" do
    it "works" do
      run [jsl| 0 'a' 'b' 'c' { pop 1 + } app3 |] `shouldBe` (((((), 0), 1), 1), 1 :: Int)
  
  describe "cleave" do
    it "works" do
      run [jsl| 'a' 6 { pop } { 2 #div } cleave |] `shouldBe` ((((), 'a'), 'a'), 3 :: Int)
  
  describe "ifte" do
    it "works" do
      let test = [jsl| { 5 >= } { 5 - } { id } ifte |]
      runOn ((), 3) test `shouldBe` ((), 3 :: Int)
      runOn ((), 6) test `shouldBe` ((), 1 :: Int)
  
  describe "whiledo" do
    it "works" do
      run [jsl| 10 { 0 >= } { 2 - } whiledo |] `shouldBe` ((), -2 :: Int)
  
  describe "tailrec" do
    it "calculates fibonacci numbers" do
      let fib = [jsl| 1 0 { pop2 0 <= } { { pop2 } dip } { { 1 - } dipd swap dupd + } tailrec |] 
      let fibF n = snd (runOn ((), n :: Int) fib) :: Int
      Prelude.map fibF [0..6] `shouldBe` [0,1,1,2,3,5,8]
  
  describe "linrec" do
    return ()

  describe "linrec'" do
    it "calculates factorials" do
      let fac = [jsl| { 0 <= } { pop 1 } { dup 1 - } { * } linrec' |]
      let facF n = snd (runOn ((), n :: Int) fac) :: Int
      Prelude.map facF [0..6] `shouldBe` [1,1,2,6,24,120,720]
     

  
  

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
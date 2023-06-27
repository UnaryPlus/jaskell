{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
module Jaskell.PreludeSpec (spec) where

import qualified Prelude
import Prelude hiding (map, filter, any, all, zipWith)

import Test.Hspec (Spec, it, describe, shouldBe)
import Jaskell.Quote (jsl)
import Jaskell (run, runOn)
import Jaskell.Prelude

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
  
  describe "Jaskell.Prelude.cons" do
    it "works" do
      run [jsl| 0 [1,2,3] cons |] `shouldBe` ((), [0,1,2,3 :: Int])
  
  describe "Jaskell.Prelude.swons" do
    it "works" do
      run [jsl| [1,2,3] 0 swons |] `shouldBe` ((), [0,1,2,3 :: Int])
  
  describe "Jaskell.Prelude.uncons" do
    return ()

  describe "Jaskell.Prelude.unswons" do
    return ()
  
  describe "Jaskell.Prelude.conjoin" do
    it "works" do
      let test = [jsl| { 3 > } { 5 < } conjoin i |]
      runOn ((), 3) test `shouldBe` (((), 3 :: Int), False)
      runOn ((), 4) test `shouldBe` (((), 4 :: Int), True)
      runOn ((), 5) test `shouldBe` (((), 5 :: Int), False)
    
    it "short circuits" do
      run [jsl| 0 { 1 > } { undefined } conjoin i |] `shouldBe` (((), 0 :: Int), False)
  
  describe "Jaskell.Prelude.disjoin" do
    it "works" do
      let test = [jsl| { 7 <= } { 9 >= } disjoin i |]
      runOn ((), 7) test `shouldBe` (((), 7 :: Int), True)
      runOn ((), 8) test `shouldBe` (((), 8 :: Int), False)
      runOn ((), 9) test `shouldBe` (((), 9 :: Int), True)
    
    it "short circuits" do
      run [jsl| 0 { 1 < } { undefined } disjoin i |] `shouldBe` (((), 0 :: Int), True)
  
  describe "Jaskell.Prelude.i" do
    it "works" do
      run [jsl| 'a' 'b' { pop 'c' } i |] `shouldBe` (((), 'a'), 'c')
  
  describe "Jaskell.Prelude.comp" do
    it "works" do
      run [jsl| 'a' 'b' { pop } { 'c' } comp i |] `shouldBe` (((), 'a'), 'c')
    
  describe "Jaskell.Prelude.consQ" do
    it "works" do
      run [jsl| 'a' 'b' { pop } { { 'c' } comp } consQ i i |] `shouldBe` (((), 'a'), 'c')
  
  describe "Jaskell.Prelude.swonsQ" do
    it "works" do
      run [jsl| 'a' 'b' { { 'c' } comp i } { pop }  swonsQ i |] `shouldBe` (((), 'a'), 'c')

  describe "Jaskell.Prelude.nullary" do
    it "works" do
      run [jsl| 5 7 { + } nullary |] `shouldBe` ((((), 5), 7), 12 :: Int)

  describe "Jaskell.Prelude.dip" do
    it "works" do
      run [jsl| 'a' 'b' 'c' { newstack } dip |] `shouldBe` ((), 'c')
  
  describe "Jaskell.Prelude.dipd" do
    it "works" do
      run [jsl| 'a' 'b' 'c' 'd' { swap } dipd |] `shouldBe` (((((), 'b'), 'a'), 'c'), 'd')
  
  describe "Jaskell.Prelude.dipdd" do
    it "works" do
      run [jsl| 'a' 'b' 'c' 'd' { pop 'x' } dipdd |] `shouldBe` (((((), 'x'), 'b'), 'c'), 'd')
  
  describe "Jaskell.Prelude.app1" do
    it "works" do
      run [jsl| 'a' 'b' { pop } app1 |] `shouldBe` (((), 'a'), 'a')
  
  describe "Jaskell.Prelude.app2" do
    it "works" do
      run [jsl| 4 5 { dup * } app2 |] `shouldBe` (((), 16), 25 :: Int)
  
  describe "Jaskell.Prelude.app3" do
    it "works" do
      run [jsl| 0 'a' 'b' 'c' { pop 1 + } app3 |] `shouldBe` (((((), 0), 1), 1), 1 :: Int)
  
  describe "Jaskell.Prelude.cleave" do
    it "works" do
      run [jsl| 'a' 6 { pop } { 2 #div } cleave |] `shouldBe` ((((), 'a'), 'a'), 3 :: Int)
  
  describe "Jaskell.Prelude.ifte" do
    it "works" do
      let test = [jsl| { 5 >= } { 5 - } { } ifte |]
      runOn ((), 3) test `shouldBe` ((), 3 :: Int)
      runOn ((), 6) test `shouldBe` ((), 1 :: Int)
  
  describe "Jaskell.Prelude.branch" do
    it "converts bools to ints" do
      let int = [jsl| { 1 } { 0 } branch |]
      let intF b = snd (runOn ((), b) int) :: Int
      intF True `shouldBe` 1
      intF False `shouldBe` 0
  
  describe "Jaskell.Prelude.cond" do
    it "works" do
      let test = [jsl| [ ({ -1 < }, { dup * }), ({ 1 > }, { dup * $negate }) ] { $negate } cond |]
      runOn ((), -2) test `shouldBe` ((), 4 :: Double)
      runOn ((), 3) test `shouldBe` ((), -9)
      runOn ((), 0.5) test `shouldBe` ((), -0.5)
  
  describe "Jaskell.Prelude.infra" do
    it "works" do
      run [jsl| 'a' () { 5 3 7 + } infra unstack |] `shouldBe` (((), 5 :: Int), 10 :: Int)
  
  describe "Jaskell.Prelude.whiledo" do
    it "works" do
      run [jsl| 10 { 0 >= } { 2 - } whiledo |] `shouldBe` ((), -2 :: Int)
  
  describe "Jaskell.Prelude.tailrec" do
    it "calculates fibonacci numbers" do
      let fib = [jsl| 1 0 { pop2 0 <= } { { pop2 } dip } { { 1 - } dipd swap dupd + } tailrec |] 
      let fibF n = snd (runOn ((), n :: Int) fib) :: Int
      Prelude.map fibF [0..6] `shouldBe` [0,1,1,2,3,5,8]
  
  describe "Jaskell.Prelude.linrec" do
    return ()

  describe "Jaskell.Prelude.linrec'" do
    it "calculates factorials" do
      let fac = [jsl| { 0 <= } { pop 1 } { dup { 1 - } dip } { * } linrec' |]
      let facF n = snd (runOn ((), n) fac) :: Int
      Prelude.map facF [0..6] `shouldBe` [1,1,2,6,24,120,720]
  
  describe "Jaskell.Prelude.binrec" do
    return ()
  
  describe "Jaskell.Prelude.binrec'" do
    it "can implement quicksort" do
      let qsort = [jsl|
            DEF small = { $null } { uncons $null } disjoin ;
            small { } { uncons { < } split rolldown }
            { swap cons ++ } binrec'
            |]
      let qsortF xs = snd (runOn ((), xs) qsort) :: [Int]
      qsortF [] `shouldBe` []
      qsortF [5] `shouldBe` [5]
      qsortF [3,5,1,6,4,2] `shouldBe` [1,2,3,4,5,6]
  
  describe "Jaskell.Prelude.natrec" do
    it "calculates factorials" do
      let fac = [jsl| { 1 } { * } natrec |]  
      let facF n = snd (runOn ((), n) fac) :: Int
      Prelude.map facF [0..6] `shouldBe` [1,1,2,6,24,120,720]
  
  describe "Jaskell.Prelude.listrec" do
    it "reverses lists" do
      let rev = [jsl| { [] } { swap [] cons ++ } listrec |]
      let revF xs = snd (runOn ((), xs) rev)
      revF [] `shouldBe` ([] :: [()])
      revF "abc" `shouldBe` "cba"
  
  describe "Jaskell.Prelude.condlinrec" do
    return ()
  
  describe "Jaskell.Prelude.times" do
    it "computes powers of two" do
      let pow = [jsl| 1 swap { 2 * } times |]
      let powF n = snd (runOn ((), n) pow) :: Int
      powF 0 `shouldBe` 1
      powF 5 `shouldBe` 32
    
  describe "Jaskell.Prelude.step" do
    it "reverses lists" do
      let rev = [jsl| [] swap { swons } step |]
      let revF xs = snd (runOn ((), xs) rev)
      revF [] `shouldBe` ([] :: [()])
      revF "abc" `shouldBe` "cba"
    
  describe "Jaskell.Prelude.step2" do
    it "works" do
      run [jsl| "" [1,2,3] "abc" { #replicate ++ } step2 |] `shouldBe` ((), "abcaabbccaaabbbccc")

  describe "Jaskell.Prelude.map" do
    it "works" do
      run [jsl| [0,1,2,3] { 2 * } map |] `shouldBe` ((), [0,2,4,6 :: Int])

  describe "Jaskell.Prelude.mapS" do
    it "works" do
      run [jsl| 0 "abcd" { dupd pair { 1 + } dip } mapS |] 
        `shouldBe` (((), 4 :: Int), [(0,'a'),(1,'b'),(2,'c'),(3,'d')])
  
  describe "Jaskell.Prelude.filter" do
    it "works" do
      run [jsl| [0,1,3,2,5,0,3] { $even } filter |] `shouldBe` ((), [0,2,0 :: Int])

  describe "Jaskell.Prelude.filterS" do
    it "works" do
      run [jsl| 0 "abbabaacbbba" { 'a' /= dup { { 1 + } dip } { id } branch } filterS |]
        `shouldBe` (((), 7 :: Int), "bbbcbbb")
  
  describe "Jaskell.Prelude.split" do
    it "works" do
      run [jsl| [-2,0,5,-3,-1,4,1] { 0 >= } split |] `shouldBe` (((), [-2,-3,-1]), [0,5,4,1 :: Int])
  
  describe "Jaskell.Prelude.splitS" do
    it "works" do
      run [jsl| 0 "abbacbaacbbba" { 'a' /= dup { { 1 + } dip } { id } branch } splitS |]
        `shouldBe` ((((), 8 :: Int), "aaaaa"), "bbcbcbbb")
  
  describe "Jaskell.Prelude.any" do
    it "detects negative numbers" do
      let neg = [jsl| { 0 < } any |]
      let negF xs = snd (runOn ((), xs :: [Int]) neg)
      negF [] `shouldBe` False
      negF [0,1,2] `shouldBe` False
      negF [0,-1,2] `shouldBe` True
      negF [0,-1,undefined] `shouldBe` True
    
  describe "Jaskell.Prelude.all" do
    it "detects repetitions" do
      let rep = [jsl| { == } all popd |]
      let repF x xs = snd (runOn (((), x), xs) rep)
      repF 'a' "" `shouldBe` True
      repF 'a' "aaa" `shouldBe` True
      repF 'b' "bab" `shouldBe` False
      repF 'b' ['b','a',undefined] `shouldBe` False
  
  describe "Jaskell.Prelude.zipwith" do
    it "works" do
      run [jsl| [0,6,5] [2,-3,1,0] { + } zipwith |] `shouldBe` ((), [2,3,6 :: Int])
  
  describe "Jaskell.Prelude.zipwithS" do
    it "works" do
      run [jsl| 0 [0,6,5] [2,-3,1,0] { + dupd + { 2 + } dip } zipwithS |]
        `shouldBe` (((), 6), [2,5,10 :: Int])
  

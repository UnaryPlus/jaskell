{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Arrows #-}
module Jaskell.Prelude 
  ( stack, unstack, newstack
  , pop, dup, swap, popd , pop2 , dupd, dup2, swapd, rollup, rolldown
  , choice, select
  , pair, unpair
  , cons, swons, uncons
  , conjoin, disjoin
  , i, comp
  , consQ, swonsQ
  , nullary, dip, dipd, dipdd
  , app1, app2, app3, cleave
  , ifte, whiledo
  , tailrec, linrec, linrec', binrec, binrec', natrec, listrec
  , cond, CLROption(..), condlinrec
  , branch, times, infra
  , step, step2, map, mapS, filter, filterS, split, splitS
  , any, all, zipwith, zipwithS
  ) where

import qualified Prelude
import Prelude hiding (map, filter, any, all, zipWith)
import Data.List (partition)
import Control.Applicative (liftA2)
import Control.Arrow (Arrow, ArrowApply, ArrowChoice, arr, (>>>), (>>^), (^>>), (&&&), returnA, app)
import qualified Data.Bifunctor as Bifunctor

stack :: Arrow arr => arr s (s, s)
stack = arr \s -> (s, s)

unstack :: Arrow arr => arr (s, a) a
unstack = arr snd

newstack :: Arrow arr => arr a ()
newstack = arr (const ())

pop :: Arrow arr => arr (s, a) s
pop = arr fst

dup :: Arrow arr => arr (s, a) ((s, a), a)
dup = arr \(s, x) -> ((s, x), x)

swap :: Arrow arr => arr ((s, a), b) ((s, b), a)
swap = arr \((s, x), y) -> ((s, y), x)

popd :: Arrow arr => arr ((s, a), b) (s, b)
popd = arr \((s, _), y) -> (s, y)

pop2 :: Arrow arr => arr ((s, a), b) s
pop2 = arr (fst . fst)

dupd :: Arrow arr => arr ((s, a), b) (((s, a), a), b)
dupd = arr \((s, x), y) -> (((s, x), x), y)

dup2 :: Arrow arr => arr ((s, a), b) ((((s, a), b), a), b)
dup2 = arr \((s, x), y) -> ((((s, x), y), x), y)

swapd :: Arrow arr => arr (((s, a), b), c) (((s, b), a), c)
swapd = arr \(((s, x), y), z) -> (((s, y), x), z)

rollup :: Arrow arr => arr (((s, a), b), c) (((s, c), a), b)
rollup = arr \(((s, x), y), z) -> (((s, z), x), y)

rolldown :: Arrow arr => arr (((s, a), b), c) (((s, b), c), a)
rolldown = arr \(((s, x), y), z) -> (((s, y), z), x)

choice :: Arrow arr => arr (((s, Bool), a), a) (s, a)
choice = arr \(((s, b), x), y) -> (s, if b then x else y)

select :: (Arrow arr, Eq a) => arr (((s, a), [(a, b)]), b) (s, b)
select = arr \(((s, x), ps), dft) -> 
  let test (x', y) z = if x == x' then y else z
  in (s, foldr test dft ps)

pair :: Arrow arr => arr ((s, a), b) (s, (a, b))
pair = arr \((s, x), y) -> (s, (x, y))

unpair :: Arrow arr => arr (s, (a, b)) ((s, a), b)
unpair = arr \(s, (x, y)) -> ((s, x), y)

cons :: Arrow arr => arr ((s, a), [a]) (s, [a])
cons = arr \((s, x), xs) -> (s, x : xs)

swons :: Arrow arr => arr ((s, [a]), a) (s, [a])
swons = arr \((s, xs), x) -> (s, x : xs)

uncons :: Arrow arr => arr (s, [a]) ((s, a), [a])
uncons = arr \(s, xs) -> 
  case xs of
    [] -> error "Jaskell.Prelude.uncons: empty list"
    x:xt -> ((s, x), xt)

conjoin :: (Arrow arr, Arrow arr') => arr ((s, arr' t (u1, Bool)), arr' t (u2, Bool)) (s, arr' t (t, Bool))
conjoin = arr \((s, p1), p2) -> 
  let p3 = returnA &&& ((p1 >>^ snd) &&& (p2 >>^ snd) >>^ uncurry (&&))
  in (s, p3)

disjoin :: (Arrow arr, Arrow arr') => arr ((s, arr' t (u1, Bool)), arr' t (u2, Bool)) (s, arr' t (t, Bool))
disjoin = arr \((s, p1), p2) -> 
  let p3 = returnA &&& ((p1 >>^ snd) &&& (p2 >>^ snd) >>^ uncurry (||))
  in (s, p3)

i :: ArrowApply arr => arr (s, arr s t) t
i = (arr \(s, f) -> (f, s)) >>> app

comp :: (Arrow arr, Arrow arr') => arr ((s, arr' a b), arr' b c) (s, arr' a c)
comp = arr \((s, f), g) -> (s, f >>> g)

consQ :: (Arrow arr, Arrow arr') => arr ((s, a), arr' (t, a) c) (s, arr' t c)
consQ = arr \((s, x), f) -> (s, (, x) ^>> f)

swonsQ :: (Arrow arr, Arrow arr') => arr ((s, arr' (t, a) c), a) (s, arr' t c)
swonsQ = arr \((s, f), x) -> (s, (, x) ^>> f)

nullary :: ArrowApply arr => arr (s, arr s (t, a)) (s, a)
nullary = proc (s, f) -> do
  (_, x) <- f -<< s
  returnA -< (s, x)

dip :: ArrowApply arr => arr ((s, a), arr s t) (t, a)
dip = proc ((s, x), f) -> do
  s' <- f -<< s
  returnA -< (s', x)

dipd :: ArrowApply arr => arr (((s, a), b), arr s t) ((t, a), b)
dipd = proc (((s, x), y), f) -> do
  s' <- f -<< s
  returnA -< ((s', x), y)

dipdd :: ArrowApply arr => arr ((((s, a), b), c), arr s t) (((t, a), b), c)
dipdd = proc ((((s, x), y), z), f) -> do
  s' <- f -<< s
  returnA -< (((s', x), y), z)

app1 :: ArrowApply arr => arr ((s, a), arr (s, a) (t, b)) (s, b)
app1 = proc ((s, x), f) -> do
  (_, x') <- f -<< (s, x)
  returnA -< (s, x')

app2 :: ArrowApply arr => arr (((s, a), a), arr (s, a) (t, b)) ((s, b), b)
app2 = proc (((s, x), y), f) -> do
  (_, x') <- f -<< (s, x)
  (_, y') <- f -<< (s, y)
  returnA -< ((s, x'), y')

app3 :: ArrowApply arr => arr ((((s, a), a), a), arr (s, a) (t, b)) (((s, b), b), b)
app3 = proc ((((s, x), y), z), f) -> do
  (_, x') <- f -<< (s, x) 
  (_, y') <- f -<< (s, y)
  (_, z') <- f -<< (s, z)
  returnA -< (((s, x'), y'), z')

cleave :: ArrowApply arr => arr (((s, a), arr (s, a) (t1, b1)), arr (s, a) (t2, b2)) ((s, b1), b2)
cleave = proc (((s, x), f), g) -> do
  (_, x1) <- f -<< (s, x)
  (_, x2) <- g -<< (s, x)
  returnA -< ((s, x1), x2) 

ifte :: ArrowApply arr => arr (((s, arr s (t, Bool)), arr s u), arr s u) u
ifte = proc (((s, p), f), g) -> do
  (_, b) <- p -<< s
  (if b then f else g) -<< s
  
whiledo :: (ArrowApply arr, ArrowChoice arr) => arr ((s, arr s (t, Bool)), arr s s) s
whiledo = proc ((s, p), f) -> do
  (_, b) <- p -<< s
  if b 
    then do
      s' <- f -<< s
      whiledo -< ((s', p), f)
    else returnA -< s

tailrec :: (ArrowApply arr, ArrowChoice arr) => arr (((s, arr s (t, Bool)), arr s u), arr s s) u
tailrec = proc (((s, p), f), g) -> do
  (_, stop) <- p -<< s
  if stop
    then f -<< s 
    else do
      s' <- g -<< s
      tailrec -< (((s', p), f), g)

linrec :: (ArrowApply arr, ArrowChoice arr) => arr ((((s, arr s (t, Bool)), arr s u), arr s s), arr u u) u
linrec = proc ((((s, p), f), g), h) -> do
  (_, stop) <- p -<< s
  if stop
    then f -<< s
    else do
      s' <- g -<< s
      u <- linrec -< ((((s', p), f), g), h)
      h -<< u

linrec' :: (ArrowApply arr, ArrowChoice arr) => arr ((((s, arr s (t, Bool)), arr s u), arr s (s, c)), arr (u, c) u) u
linrec' = proc ((((s, p), f), g), h) -> do
  (_, stop) <- p -<< s
  if stop
    then f -<< s
    else do
      (s', x) <- g -<< s
      u <- linrec' -< ((((s', p), f), g), h)
      h -<< (u, x)

binrec :: (ArrowApply arr, ArrowChoice arr) => arr (((((s, a), arr (s, a) (t, Bool)), arr (s, a) (u, b)), arr (s, a) ((s, a), a)), arr ((s, b), b) (u, b)) (u, b)
binrec = proc ((((s, p), f), g), h) -> do
  (_, stop) <- p -<< s
  if stop
    then f -<< s
    else do
      ((s', x), y) <- g -<< s
      (_, x') <- binrec -< (((((s', x), p), f), g), h)
      (_, y') <- binrec -< (((((s', y), p), f), g), h)
      h -<< ((s', x'), y')

binrec' :: (ArrowApply arr, ArrowChoice arr) => arr (((((s, a), arr (s, a) (t, Bool)), arr (s, a) (u, b)), arr (s, a) (((s, a), a), c)), arr (((s, b), b), c) (u, b)) (u, b)
binrec' = proc ((((s, p), f), g), h) -> do
  (_, stop) <- p -<< s
  if stop
    then f -<< s
    else do
      (((s', x), y), z) <- g -<< s
      (_, x') <- binrec -< (((((s', x), p), f), g), h)
      (_, y') <- binrec -< (((((s', y), p), f), g), h)
      h -<< (((s', x'), y'), z)

----------
-- genrec?
----------

natrec :: (ArrowApply arr, ArrowChoice arr) => arr (((s, Int), arr s (t, b)), arr ((s, Int), b) (t, b)) (t, b)
natrec = proc (((s, n), f), g) -> 
  if n <= 0 
    then f -<< s
    else do
      (_, res) <- natrec -< (((s, n - 1), f), g)
      g -<< ((s, n), res)

listrec :: (ArrowApply arr, ArrowChoice arr) => arr (((s, [a]), arr s (t, b)), arr ((s, a), b) (t, b)) (t, b)
listrec = proc (((s, xs), f), g) ->
  case xs of
    [] -> f -<< s
    x:xt -> do
      (_, res) <- listrec -< (((s, xt), f), g)
      g -<< ((s, x), res)

-- private
chooseA :: (ArrowApply arr, ArrowChoice arr) => arr (s, [(arr s (t, Bool), a)], a) a
chooseA = proc (s, ps, dft) -> 
  case ps of
    [] -> returnA -< dft
    (p, x):pt -> do
      (_, b) <- p -<< s
      if b 
        then returnA -< x 
        else chooseA -< (s, pt, dft) 

cond :: (ArrowApply arr, ArrowChoice arr) => arr ((s, [(arr s (t, Bool), arr s u)]), arr s u) u
cond = proc ((s, ps), dft) -> do
  f <- chooseA -< (s, ps, dft)
  f -<< s

data CLROption arr s u 
  = Stop (arr s u)
  | Recurse (arr s s) (arr u u)

condlinrec :: (ArrowApply arr, ArrowChoice arr) => arr ((s, [(arr s (t, Bool), CLROption arr s u)]), CLROption arr s u) u 
condlinrec = proc ((s, ps), dft) -> do
  opt <- chooseA -< (s, ps, dft)
  case opt of
    Stop f -> f -<< s
    Recurse f g -> do
      s' <- f -<< s
      res <- condlinrec -< ((s', ps), dft)
      g -<< res

-----------------------------
-- construct: not well typed?
-----------------------------

branch :: ArrowApply arr => arr (((s, Bool), arr s t), arr s t) t
branch = proc (((s, b), f), g) -> (if b then f else g) -<< s

times :: (ArrowApply arr, ArrowChoice arr) => arr ((s, Int), arr s s) s
times = proc ((s, n), f) -> 
  if n <= 0
    then returnA -< s
    else do
      s' <- f -<< s
      times -< ((s', n - 1), f)

infra :: ArrowApply arr => arr ((s, t), arr t u) (s, u)
infra = proc ((s, x), f) -> do
  x' <- f -<< x
  returnA -< (s, x')

step :: (ArrowApply arr, ArrowChoice arr) => arr ((s, [a]), arr (s, a) s) s
step = proc ((s, xs), f) ->
  case xs of
    [] -> returnA -< s
    x:xt -> do
      s' <- f -<< (s, x)
      step -< ((s', xt), f)

step2 :: (ArrowApply arr, ArrowChoice arr) => arr (((s, [a]), [b]), arr ((s, a), b) s) s
step2 = ( \(((s, xs), ys), f) -> ((s, liftA2 (,) xs ys), ( \(t, (x, y)) -> ((t, x), y) ) ^>> f) ) ^>> step

map :: Arrow arr => arr ((s, [a]), (s, a) -> (t, b)) (s, [b])
map = arr \((s, xs), f) -> (s, Prelude.map (\x -> snd (f (s, x))) xs)

mapS :: (ArrowApply arr, ArrowChoice arr) => arr ((s, [a]), arr (s, a) (s, b)) (s, [b])
mapS = proc ((s, xs), f) ->
  case xs of
    [] -> returnA -< (s, [])
    x:xt -> do
      (s', y) <- f -<< (s, x)
      (s'', yt) <- mapS -< ((s', xt), f)
      returnA -< (s'', y:yt)

----------------------
-- fold: just use step
----------------------

filter :: Arrow arr => arr ((s, [a]), (s, a) -> (t, Bool)) (s, [a])
filter = arr \((s, xs), f) -> (s, Prelude.filter (\x -> snd (f (s, x))) xs)

filterS :: (ArrowApply arr, ArrowChoice arr) => arr ((s, [a]), arr (s, a) (s, Bool)) (s, [a])
filterS = proc ((s, xs), f) -> 
  case xs of
    [] -> returnA -< (s, [])
    x:xt -> do
      (s', b) <- f -<< (s, x)
      (s'', yt) <- filterS -< ((s', xt), f)
      returnA -< (s'', if b then x:yt else yt)

split :: Arrow arr => arr ((s, [a]), (s, a) -> (t, Bool)) ((s, [a]), [a])
split = arr \((s, xs), f) ->
  let (trues, falses) = partition (\x -> snd (f (s, x))) xs
  in ((s, falses), trues)

splitS :: (ArrowApply arr, ArrowChoice arr) => arr ((s, [a]), arr (s, a) (s, Bool)) ((s, [a]), [a])
splitS = proc ((s, xs), f) ->
  case xs of
    [] -> returnA -< ((s, []), [])
    x:xt -> do
      (s', b) <- f -<< (s, x)
      ((s'', falses), trues) <- splitS -< ((s', xt), f)
      returnA -< if b then ((s'', falses), x:trues) else ((s'', x:falses), trues) 

any :: (ArrowApply arr, ArrowChoice arr) => arr ((s, [a]), arr (s, a) (t, Bool)) (s, Bool)
any = proc ((s, xs), f) -> 
  case xs of
    [] -> returnA -< (s, False)
    x:xt -> do
      (_, b) <- f -<< (s, x)
      if b 
        then returnA -< (s, True) 
        else any -< ((s, xt), f)

all :: (ArrowApply arr, ArrowChoice arr) => arr ((s, [a]), arr (s, a) (t, Bool)) (s, Bool)
all = proc ((s, xs), f) ->
  case xs of
    [] -> returnA -< (s, True)
    x:xt -> do
      (_, b) <- f -<< (s, x)
      if b
        then all -< ((s, xt), f)
        else returnA -< (s, False)

zipwith :: Arrow arr => arr (((s, [a]), [b]), ((s, a), b) -> (t, c)) (s, [c])
zipwith = arr \(((s, xs), ys), f) -> (s, Prelude.zipWith (\x y -> snd (f ((s, x), y))) xs ys)

zipwithS :: (ArrowApply arr, ArrowChoice arr) => arr (((s, [a]), [b]), arr ((s, a), b) (s, c)) (s, [c])
zipwithS = proc (((s, xs), ys), f) -> 
  case (xs, ys) of
    ([], _) -> returnA -< (s, [])
    (_, []) -> returnA -< (s, [])
    (x:xt, y:yt) -> do
      (s', z) <- f -<< ((s, x), y)
      (s'', zt) <- zipwithS -< (((s', xt), yt), f)
      returnA -<< (s'', z:zt)

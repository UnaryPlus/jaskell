{-# LANGUAGE BlockArguments #-}
module Jaskell.Prelude 
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
  ) where

import qualified Prelude
import Prelude hiding (map, filter, any, all, zipWith)
import Data.List (foldl', partition)
import Control.Applicative (liftA2)
import Control.Arrow (Arrow, arr)

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

conjoin :: Arrow arr => arr ((s, t -> (u1, Bool)), t -> (u2, Bool)) (s, t -> (t, Bool))
conjoin = arr \((s, p1), p2) -> (s, \t -> (t, snd (p1 t) && snd (p2 t)))

disjoin :: Arrow arr => arr ((s, t -> (u1, Bool)), t -> (u2, Bool)) (s, t -> (t, Bool))
disjoin = arr \((s, p1), p2) -> (s, \t -> (t, snd (p1 t) || snd (p2 t)))

i :: Arrow arr => arr (s, s -> t) t
i = arr \(s, f) -> f s

comp :: Arrow arr => arr ((s, a -> b), b -> c) (s, a -> c)
comp = arr \((s, f), g) -> (s, g . f)

consQ :: Arrow arr => arr ((s, a), (t, a) -> c) (s, t -> c)
consQ = arr \((s, x), f) -> (s, \t -> f (t, x))

swonsQ :: Arrow arr => arr ((s, (t, a) -> c), a) (s, t -> c)
swonsQ = arr \((s, f), x) -> (s, \t -> f (t, x))

nullary :: Arrow arr => arr (s, s -> (t, a)) (s, a)
nullary = arr \(s, f) -> (s, snd (f s))

dip :: Arrow arr => arr ((s, a), s -> t) (t, a)
dip = arr \((s, x), f) -> (f s, x)

dipd :: Arrow arr => arr (((s, a), b), s -> t) ((t, a), b)
dipd = arr \(((s, x), y), f) -> ((f s, x), y)

dipdd :: Arrow arr => arr ((((s, a), b), c), s -> t) (((t, a), b), c)
dipdd = arr \((((s, x), y), z), f) -> (((f s, x), y), z)

-- private
top :: ((a, b) -> (c, d)) -> a -> b -> d
top f s x = snd (f (s, x))

app1 :: Arrow arr => arr ((s, a), (s, a) -> (t, b)) (s, b)
app1 = arr \((s, x), f) -> (s, top f s x)

app2 :: Arrow arr => arr (((s, a), a), (s, a) -> (t, b)) ((s, b), b)
app2 = arr \(((s, x), y), f) -> ((s, top f s x), top f s y)

app3 :: Arrow arr => arr ((((s, a), a), a), (s, a) -> (t, b)) (((s, b), b), b)
app3 = arr \((((s, x), y), z), f) -> (((s, top f s x), top f s y), top f s z)

cleave :: Arrow arr => arr (((s, a), (s, a) -> (t1, b1)), (s, a) -> (t2, b2)) ((s, b1), b2)
cleave = arr \(((s, x), f), g) -> ((s, top f s x), top g s x)

ifte :: Arrow arr => arr (((s, s -> (t, Bool)), s -> u), s -> u) u
ifte = arr \(((s, p), f), g) -> if snd (p s) then f s else g s 

whiledo :: Arrow arr => arr ((s, s -> (t, Bool)), s -> s) s
whiledo = arr \((s, p), f) -> 
  if snd (p s) then whiledo ((f s, p), f)
  else s

tailrec :: Arrow arr => arr (((s, s -> (t, Bool)), s -> u), s -> s) u
tailrec = arr \(((s, p), f), g) ->
  if snd (p s) then f s
  else tailrec (((g s, p), f), g)

linrec :: Arrow arr => arr ((((s, s -> (t, Bool)), s -> u), s -> s), u -> u) u
linrec = arr \((((s, p), f), g), h) ->
  if snd (p s) then f s
  else h (linrec ((((g s, p), f), g), h))

binrec :: Arrow arr => arr (((((s, a), (s, a) -> (t, Bool)), (s, a) -> (u, b)), (s, a) -> ((s, a), a)), ((s, b), b) -> (u, b)) (u, b)
binrec = arr \((((s, p), f), g), h) ->
  if snd (p s) then f s
  else let 
    ((s', x), y) = g s
    (_, x') = binrec (((((s', x), p), f), g), h)
    (_, y') = binrec (((((s', y), p), f), g), h)
    in h ((s', x'), y')

----------
-- genrec?
----------

natrec :: Arrow arr => arr (((s, Int), s -> (t, b)), ((s, Int), b) -> (t, b)) (t, b)
natrec = arr \(((s, n), f), g) ->
  if n <= 0 then f s
  else g ((s, n), snd (natrec (((s, n - 1), f), g)))

listrec :: Arrow arr => arr (((s, [a]), s -> (t, b)), ((s, a), b) -> (t, b)) (t, b)
listrec = arr \(((s, xs), f), g) ->
  case xs of
    [] -> f s
    x:xt -> g ((s, x), snd (listrec (((s, xt), f), g)))

cond :: Arrow arr => arr ((s, [(s -> (t, Bool), s -> u)]), s -> u) u
cond = arr \((s, ps), dft) -> 
  let test (p, f) f' = if snd (p s) then f else f'
  in foldr test dft ps s

data CLROption s u 
  = Stop (s -> u)
  | Recurse (s -> s) (u -> u)

condlinrec :: Arrow arr => arr ((s, [(s -> (t, Bool), CLROption s u)]), CLROption s u) u 
condlinrec = arr \((s, ps), dft) -> 
  let test (p, f) f' = if snd (p s) then interpret f else f'
      interpret (Stop f) = f s
      interpret (Recurse f g) = g (condlinrec ((f s, ps), dft))
  in foldr test (interpret dft) ps

-----------------------------
-- construct: not well typed?
-----------------------------

branch :: Arrow arr => arr (((s, Bool), s -> t), s -> t) t
branch = arr \(((s, b), f), g) -> if b then f s else g s

times :: Arrow arr => arr ((s, Int), s -> s) s
times = arr \((s, n), f) -> 
  let go k s' = if k <= 0 then s' else go (k - 1) (f s')
  in go n s

infra :: Arrow arr => arr ((s, t), t -> u) (s, u)
infra = arr \((s, x), f) -> (s, f x)

step :: Arrow arr => arr ((s, [a]), (s, a) -> s) s
step = arr \((s, xs), f) -> foldl' (curry f) s xs

-- private
assoc :: (((a, b), c) -> d) -> a -> (b, c) -> d 
assoc f x (y, z) = f ((x, y), z)

step2 :: Arrow arr => arr (((s, [a]), [b]), ((s, a), b) -> s) s
step2 = arr \(((s, xs), ys), f) -> foldl' (assoc f) s (liftA2 (,) xs ys)

map :: Arrow arr => arr ((s, [a]), (s, a) -> (t, b)) (s, [b])
map = arr \((s, xs), f) -> (s, Prelude.map (\x -> snd (f (s, x))) xs)

mapS :: Arrow arr => arr ((s, [a]), (s, a) -> (s, b)) (s, [b])
mapS = arr \((s, xs), f) -> case xs of 
  [] -> (s, [])
  x:xt -> let (s', y) = f (s, x) in (y:) <$> mapS ((s', xt), f)

----------------------
-- fold: just use step
----------------------

filter :: Arrow arr => arr ((s, [a]), (s, a) -> (t, Bool)) (s, [a])
filter = arr \((s, xs), f) -> (s, Prelude.filter (\x -> snd (f (s, x))) xs)

filterS :: Arrow arr => arr ((s, [a]), (s, a) -> (s, Bool)) (s, [a])
filterS = arr \((s, xs), f) -> case xs of
  [] -> (s, [])
  x:xt -> let 
    (s', b) = f (s, x) 
    res = filterS ((s', xt), f)
    in if b then (x:) <$> res else res

split :: Arrow arr => arr ((s, [a]), (s, a) -> (t, Bool)) ((s, [a]), [a])
split = arr \((s, xs), f) ->
  let (trues, falses) = partition (\x -> snd (f (s, x))) xs
  in ((s, trues), falses)

splitS :: Arrow arr => arr ((s, [a]), (s, a) -> (s, Bool)) ((s, [a]), [a])
splitS = arr \((s, xs), f) -> case xs of
  [] -> ((s, []), [])
  x:xt -> let
    (s', b) = f (s, x)
    ((res, trues), falses) = splitS ((s', xt), f)
    in if b then ((res, x:trues), falses) else ((res, trues), x:falses)

any :: Arrow arr => arr ((s, [a]), (s, a) -> (t, Bool)) (s, Bool)
any = arr \((s, xs), f) -> (s, Prelude.any (\x -> snd (f (s, x))) xs)

all :: Arrow arr => arr ((s, [a]), (s, a) -> (t, Bool)) (s, Bool)
all = arr \((s, xs), f) -> (s, Prelude.all (\x -> snd (f (s, x))) xs)

zipwith :: Arrow arr => arr (((s, [a]), [b]), ((s, a), b) -> (t, c)) (s, [c])
zipwith = arr \(((s, xs), ys), f) -> (s, Prelude.zipWith (\x y -> snd (f ((s, x), y))) xs ys)

zipwithS :: Arrow arr => arr (((s, [a]), [b]), ((s, a), b) -> (s, c)) (s, [c])
zipwithS = arr \(((s, xs), ys), f) -> case (xs, ys) of
  ([], _) -> (s, [])
  (_, []) -> (s, [])
  (x:xt, y:yt) -> let
    (s', z) = f ((s, x), y)
    in (z:) <$> zipwithS (((s', xt), yt), f)

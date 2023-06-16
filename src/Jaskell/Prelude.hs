module Jaskell.Prelude where

import qualified Prelude
import Prelude hiding (map, filter)
import Data.List (foldl', partition)
import Control.Applicative (liftA2)

stack :: s -> (s, s)
stack s = (s, s)

unstack :: (s, a) -> a
unstack = snd

newstack :: a -> ()
newstack _ = ()

pop :: (s, a) -> s
pop = fst

dup :: (s, a) -> ((s, a), a)
dup (s, x) = ((s, x), x)

swap :: ((s, a), b) -> ((s, b), a)
swap ((s, x), y) = ((s, y), x)

popd :: ((s, a), b) -> (s, b)
popd ((s, _), y) = (s, y)

pop2 :: ((s, a), b) -> s
pop2 = fst . fst

dupd :: ((s, a), b) -> (((s, a), a), b)
dupd ((s, x), y) = (((s, x), x), y)

dup2 :: ((s, a), b) -> ((((s, a), b), a), b)
dup2 ((s, x), y) = ((((s, x), y), x), y)

swapd :: (((s, a), b), c) -> (((s, b), a), c)
swapd (((s, x), y), z) = (((s, y), x), z)

rollup :: (((s, a), b), c) -> (((s, c), a), b)
rollup (((s, x), y), z) = (((s, z), x), y)

rolldown :: (((s, a), b), c) -> (((s, b), c), a)
rolldown (((s, x), y), z) = (((s, y), z), x)

choice :: (((s, Bool), a), a) -> (s, a)
choice (((s, b), x), y) = (s, if b then x else y)

select :: Eq a => (((s, a), [(a, b)]), b) -> (s, b)
select (((s, x), ps), dft) = (s, foldr test dft ps)
  where test (x', y) z = if x == x' then y else z

pair :: ((s, a), b) -> (s, (a, b))
pair ((s, x), y) = (s, (x, y))

unpair :: (s, (a, b)) -> ((s, a), b)
unpair (s, (x, y)) = ((s, x), y)

cons :: ((s, a), [a]) -> (s, [a])
cons ((s, x), xs) = (s, x : xs)

swons :: ((s, [a]), a) -> (s, [a])
swons ((s, xs), x) = (s, x : xs)

conjoin :: ((s, t -> (u1, Bool)), t -> (u2, Bool)) -> (s, t -> (t, Bool))
conjoin ((s, p1), p2) = (s, \t -> (t, snd (p1 t) && snd (p2 t)))

disjoin :: ((s, t -> (u1, Bool)), t -> (u2, Bool)) -> (s, t -> (t, Bool))
disjoin ((s, p1), p2) = (s, \t -> (t, snd (p1 t) || snd (p2 t)))

i :: (s, s -> t) -> t
i (s, f) = f s

comp :: ((s, a -> b), b -> c) -> (s, a -> c)
comp ((s, f), g) = (s, g . f)

consq :: ((s, a), (t, a) -> c) -> (s, t -> c)
consq ((s, x), f) = (s, \t -> f (t, x))

swonsq :: ((s, (t, a) -> c), a) -> (s, t -> c)
swonsq ((s, f), x) = (s, \t -> f (t, x))

nullary :: (s, s -> (t, a)) -> (s, a)
nullary (s, f) = (s, snd (f s))

dip :: ((s, a), s -> t) -> (t, a)
dip ((s, x), f) = (f s, x)

dipd :: (((s, a), b), s -> t) -> ((t, a), b)
dipd (((s, x), y), f) = ((f s, x), y)

dipdd :: ((((s, a), b), c), s -> t) -> (((t, a), b), c)
dipdd ((((s, x), y), z), f) = (((f s, x), y), z)

-- private
top :: ((a, b) -> (c, d)) -> a -> b -> d
top f s x = snd (f (s, x))

app1 :: ((s, a), (s, a) -> (t, b)) -> (s, b)
app1 ((s, x), f) = (s, top f s x)

app2 :: (((s, a), a), (s, a) -> (t, b)) -> ((s, b), b)
app2 (((s, x), y), f) = ((s, top f s x), top f s y)

app3 :: ((((s, a), a), a), (s, a) -> (t, b)) -> (((s, b), b), b)
app3 ((((s, x), y), z), f) = (((s, top f s x), top f s y), top f s z)

cleave :: (((s, a), (s, a) -> (t1, b1)), (s, a) -> (t2, b2)) -> ((s, b1), b2)
cleave (((s, x), f), g) = ((s, top f s x), top g s x)

ifte :: (((s, s -> (t, Bool)), s -> u), s -> u) -> u
ifte (((s, p), f), g) = if snd (p s) then f s else g s 

whiledo :: ((s, s -> (t, Bool)), s -> s) -> s
whiledo ((s, p), f) = 
  if snd (p s) then whiledo ((f s, p), f)
  else s

tailrec :: (((s, s -> (t, Bool)), s -> u), s -> s) -> u
tailrec (((s, p), f), g) =
  if snd (p s) then f s
  else tailrec (((g s, p), f), g)

linrec :: ((((s, s -> (t, Bool)), s -> u), s -> s), u -> u) -> u
linrec ((((s, p), f), g), h) =
  if snd (p s) then f s
  else h (linrec ((((g s, p), f), g), h))

binrec :: (((((s, a), (s, a) -> (t, Bool)), (s, a) -> (u, b)), (s, a) -> ((s, a), a)), ((s, b), b) -> (u, b)) -> (u, b)
binrec ((((s, p), f), g), h) =
  if snd (p s) then f s
  else let 
    ((s', x), y) = g s
    (_, x') = binrec (((((s', x), p), f), g), h)
    (_, y') = binrec (((((s', y), p), f), g), h)
    in h ((s', x'), y')

-- genrec?

natrec :: (((s, Int), s -> (t, b)), ((s, Int), b) -> (t, b)) -> (t, b)
natrec (((s, n), f), g) =
  if n <= 0 then f s
  else g ((s, n), snd (natrec (((s, n - 1), f), g)))

listrec :: (((s, [a]), s -> (t, b)), ((s, a), b) -> (t, b)) -> (t, b)
listrec (((s, xs), f), g) =
  case xs of
    [] -> f s
    x:xt -> g ((s, x), snd (natrec (((s, xt), f), g)))

cond :: ((s, [(s -> (t, Bool), s -> u)]), s -> u) -> u
cond ((s, ps), dft) = (foldr test dft ps) s
  where test (p, f) f' = if snd (p s) then f else f'

data CLROption s u 
  = Stop (s -> u)
  | Recurse (s -> s) (u -> u)

condlinrec :: ((s, [(s -> (t, Bool), CLROption s u)]), CLROption s u) -> u 
condlinrec ((s, ps), dft) = foldr test (interpret dft) ps
  where test (p, f) f' = if snd (p s) then interpret f else f'
        interpret (Stop f) = f s
        interpret (Recurse f g) = g (condlinrec ((f s, ps), dft))

-- construct: not well-typed?

branch :: (((s, Bool), s -> t), s -> t) -> t
branch (((s, b), f), g) = if b then f s else g s

times :: ((s, Int), s -> s) -> s
times ((s, n), f) = go n id
  where go k g = if k <= 0 then g else go (k - 1) (f . g)

infra :: ((s, t), t -> u) -> (s, u)
infra ((s, x), f) = (s, f x)

step :: ((s, [a]), (s, a) -> s) -> s
step ((s, xs), f) = foldl' (curry f) s xs

-- private
assoc :: (((a, b), c) -> d) -> a -> (b, c) -> d 
assoc f x (y, z) = f ((x, y), z)

step2 :: (((s, [a]), [b]), ((s, a), b) -> s) -> s
step2 (((s, xs), ys), f) = foldl' (assoc f) s (liftA2 (,) xs ys)

map :: ((s, [a]), (s, a) -> (t, b)) -> (s, [b])
map ((s, xs), f) = (s, Prelude.map (\x -> snd (f (s, x))) xs)

mapS :: ((s, [a]), (s, a) -> (s, b)) -> (s, [b])
mapS ((s, xs), f) = case xs of 
  [] -> (s, [])
  x:xt -> let (s', y) = f (s, x) in (y:) <$> mapS ((s', xt), f)

filter :: ((s, [a]), (s, a) -> (t, Bool)) -> (s, [a])
filter ((s, xs), f) = (s, Prelude.filter (\x -> snd (f (s, x))) xs)

filterS :: ((s, [a]), (s, a) -> (s, Bool)) -> (s, [a])
filterS ((s, xs), f) = case xs of
  [] -> (s, [])
  x:xt -> let 
    (s', b) = f (s, x) 
    res = filterS ((s', xt), f)
    in if b then (x:) <$> res else res

split :: ((s, [a]), (s, a) -> (t, Bool)) -> ((s, [a]), [a])
split ((s, xs), f) = 
  let (trues, falses) = partition (\x -> snd (f (s, x))) xs
  in ((s, trues), falses)

splitS :: ((s, [a]), (s, a) -> (s, Bool)) -> ((s, [a]), [a])
splitS ((s, xs), f) = case xs of
  [] -> ((s, []), [])
  x:xt -> let
    (s', b) = f (s, x)
    ((res, trues), falses) = splitS ((s', xt), f)
    in if b then ((res, x:trues), falses) else ((res, trues), x:falses)
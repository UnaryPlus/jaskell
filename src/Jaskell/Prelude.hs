{-# LANGUAGE PatternSynonyms #-}

module Jaskell.Prelude () where

-- stack: not well-typed
-- unstack: not well-typed

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
swapd (((s, x), y, z)) = (((s, y), x), z)

rollup :: (((s, a), b), c) -> (((s, c), a), b)
rollup (((s, x), y), z) = (((s, z), s), y)

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

conjoin :: ((s, a -> (b, Bool)), a -> (b, Bool)) -> (s, a -> ((), Bool))
conjoin

-- conjoin
-- disjoin
-- negate

i :: (s, s -> t) -> t
i (s, f) = f s

comp :: ((s, a -> b), b -> c) -> (s, a -> c)
comp ((s, f), g) = (s, g . f)

nullary :: (s, s -> (t, a)) -> (s, a)
nullary (s, f) = (s, snd f)

dip :: ((s, a), s -> t) -> (t, a)
dip ((s, x), f) = (f s, x)

dipd :: (((s, a), b), s -> t) -> ((t, a), b)
dipd (((s, x), y), f) = ((f s, x), y)

dipdd :: ((((s, a), b), c), s -> t) -> (((t, a), b), c)
dipdd ((((s, x), y), z), f) = (((f s, x), y), z)

-- private
top :: ((a, b) -> (c, d)) -> a -> b -> c
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
  else e

tailrec :: (((s, s -> (t, Bool)), s -> u), s -> s) -> u
tailrec (((s, p), f), g) =
  if snd (p s) then f s
  else tailrec (((g s, p), f), g)

linrec :: ((((s, s -> (t, Bool)), s -> u), s -> s), u -> u) -> u
linrec ((((s, p), f), g), h) =
  if snd (p s) then f s
  else h (linrec ((((g s, p), f), g), h))

{-
binrec ((((s, p), f), g), h) = 
  if snd (p s) then f s
  else let 
    ((s', x), y) = g s
    s1 = (s', x)
    s2 = (s', y)
    s1' = binrec ((((s1, p), f), g), h)
    s2' = binrec ((((s2, p), f), g), h)
  in h s1' s2'
-}

-- binrec
-- genrec
-- primrec
-- cond  
-- condlinrec
-- construct

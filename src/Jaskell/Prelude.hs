{-# LANGUAGE PatternSynonyms #-}

pop :: (s, a) -> s
pop = fst

dup :: (s, a) -> ((s, a), a)
dup (s, x) = ((s, x), x)

swap :: ((s, a), b) -> ((s, b), a)
swap ((s, x), y) = ((s, y), x)

popd :: ((s, a), b) -> (s, b)
popd ((s, _), y) = (s, y)

dupd :: ((s, a), b) -> (((s, a), a), b)
dupd ((s, x), y) = (((s, x), x), y)

swapd :: (((s, a), b), c) -> (((s, b), a), c)
swapd (((s, x), y, z)) = (((s, y), x), z)

popop :: ((s, a), b) -> s
popop = fst . fst
 
dup2 :: ((s, a), b) -> ((((s, a), b), a), b)
dup2 ((s, x), y) = ((((s, x), y), x), y)

rollup :: (((s, a), b), c) -> (((s, c), a), b)
rollup (((s, x), y), z) = (((s, z), s), y)

rolldown :: (((s, a), b), c) -> (((s, b), c), a)
rolldown (((s, x), y), z) = (((s, y), z), x)

choice :: (((s, Bool), a), a) -> (s, a)
choice (((s, b), x), y) = (s, if b then x else y)

pair :: ((s, a), b) -> (s, (a, b))
pair ((s, x), y) = (s, (x, y))

unpair :: (s, (a, b)) -> ((s, a), b)
unpair (s, (x, y)) = ((s, x), y)

i :: (s, s -> t) -> t
i (s, f) = f s

dip :: ((s, a), s -> t) -> (t, a)
dip ((s, x), f) = (f s, x)

dipd :: (((s, a), b), s -> t) -> ((t, a), b)
dipd (((s, x), y), f) = ((f s, x), y)

dipdd :: ((((s, a), b), c), s -> t) -> (((t, a), b), c)
dipdd ((((s, x), y), z), f) = (((f s, x), y), z)

app1 :: ((s, a), (s, a) -> (t, b)) -> (s, b)
app1 ((s, x), f) = (s, snd (f (s, x)))

app2 :: (((s, a), a), (s, a) -> (t, b)) -> ((s, b), b)
app2 (((s, x), y), f) = ((s, top x), top y)
  where top q = snd (f (s, q))

app3 :: ((((s, a), a), a), (s, a) -> (t, b)) -> (((s, b), b), b)
app3 ((((s, x), y), z), f) = (((s, top x), top y), top z)
  where top q = snd (f (s, q))

-- same as `swap .` 
comp :: ((s, a -> b), b -> c) -> (s, a -> c)
comp ((s, f), g) = (s, g . f)

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






-- conjoin
-- disjoin
-- negate




-- must be followed by function/constructor name
-- $    map function over top value
-- #    map binary function over top two values
-- !    pop value and execute monadic action
-- ?    push result of monadic action
-- &    map monadic action over top value

-- operator        map operator over top two values
-- constructor     push constructor onto stack  
-- function        apply function to stack   
-- literal         (num, string, char) push value onto stack
-- [ e1, ... en ]  syntactic sugar for e1 ... en [] : ... :
-- { expr }        push translation of expr onto stack











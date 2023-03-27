module Jaskell () where

-- must be followed by function/constructor name
-- $    map function over top value
-- #    map binary function over top two values
-- &    map monadic action over top value
-- !    pop value and execute monadic action
-- ?    push result of monadic action

-- operator        map operator over top two values
-- constructor     push constructor onto stack  
-- function        apply function to stack   
-- literal         (num, string, char) push value onto stack
-- [ e1, ... en ]  syntactic sugar for e1 ... en nil cons ... cons
-- ( a, b )        syntactic sugar for a b pair
-- { expr }        push translation of expr onto stack

push :: a -> s -> (s, a)
push x s = (s, x)

liftS1 :: (a -> b) -> (s, a) -> (s, b)
liftS1 = fmap

liftS2 :: (a -> b -> c) -> ((s, a), b) -> (s, c)
liftS2 f ((s, x), y) = (s, f x y)

pushM :: Functor m => m a -> Kliesli m s (s, a)
pushM mx = Kliesli \s -> fmap ((,) s) mx

liftSM :: Monad m => (a -> m b) -> Kleisli m (s, a) (s, b)
listSM f = Kleisli (mapM f)

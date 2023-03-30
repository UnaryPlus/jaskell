{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
module Jaskell (push, liftS, liftS2, pushM, popM, liftSM) where

import Control.Arrow (Kleisli(Kleisli))

-- must be followed by function/constructor name
-- $    map function over top value
-- #    map binary function over top two values
-- ?    push result of monadic action
-- !    pop value and execute monadic action
-- &    map monadic action over top value

-- operator        map operator over top two values
-- constructor     push constructor onto stack  
-- function        apply function to stack   
-- literal         (num, string, char) push value onto stack
-- [ e1, ... en ]  syntactic sugar for e1 ... en nil cons ... cons
-- ( a, b )        syntactic sugar for a b pair
-- { expr }        push translation of expr onto stack

push :: a -> s -> (s, a)
push x s = (s, x)

liftS :: (a -> b) -> (s, a) -> (s, b)
liftS f (s, x) = (s, f x)

liftS2 :: (a -> b -> c) -> ((s, a), b) -> (s, c)
liftS2 f ((s, x), y) = (s, f x y)

pushM :: Functor m => m a -> Kleisli m s (s, a)
pushM mx = Kleisli \s -> fmap (s, ) mx

popM :: Functor m => (a -> m ()) -> Kleisli m (s, a) s
popM f = Kleisli \(s, x) -> fmap (const s) (f x)

liftSM :: Functor m => (a -> m b) -> Kleisli m (s, a) (s, b)
liftSM f = Kleisli \(s, x) -> fmap (s, ) (f x)
